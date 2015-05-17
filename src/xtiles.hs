import qualified System.Environment as Env (getArgs, getProgName)
import           Text.XML.HXT.Core
-- import           Text.XML.HXT.Arrow.ReadDocument (xreadDoc)

---------------------------------------------
-- constants
---------------------------------------------

verbose :: Bool
verbose = True

---------------------------------------------
-- expressions
---------------------------------------------

{-
data TLiteral    = LString String | LInteger Integer | LBool Bool deriving (Eq, Show)
data TIdent      = Ident String
data TFunCall    = FunCall { funcName :: String, funcParams :: [TExpr] } deriving (Show)
data TExpr       = TLiteral | TIdent | TFunCall | CmpEq TExpr TExpr deriving (Show)

-- type expressions are used with const/param declarations; TODO: elem list? attr list? nodeset?
data TTypeExpr   = UTypeString | UTypeInteger | UTypeBool | UTypeFile | UTypeXPath deriving (Eq, Show)
-}

---------------------------------------------
-- runtime
---------------------------------------------

{-
data TStackFrame = StackFrame {
    fileName :: String,
    lineNo   :: Integer
} deriving (Show)

data TError = Error {
    stack    :: [TStackFrame],
    errorMsg :: String
} deriving (Show)

-- identifier binding
data TIdentBinding = IdentBinding {
    origExpr  :: TExpr,                            -- original expression
    exprValue :: Either TError TRunValue           -- error or computed value
} deriving (Show)

-- processing phase; TODO is this necessary?...
data TPhase = PhaseIORead | PhaseIOWrite

-- identifier values
data TRunValue = VString String | VInteger Integer | VBool Bool | VFile String deriving (Eq, Show)

-- file contents
data TFileContents = FileContents {
    fileContentsStr :: Maybe String,
    fileContentsXml :: Maybe XmlTree
}

data TRunState = RunState {
    errorInfo    :: TError,
    phase        :: TPhase,
    identMap     :: Map.Map String TIdentBinding,               -- identifier name -> value
    fileContents :: Map.Map String TFileContents                -- file name -> file contents
}

-- TODO
processTemplate :: IOStateArrow TRunState XmlTree XmlTree
processTemplate = validateDocument
-- processTemplate state xmlTree = return (state, [xmlTree])
-}

---------------------------------------------
-- types
-- TODO nasty things (does it have to be like this?):
-- 1. lack of type unions (lots of nested constructors instead)
-- 2. record field names must be unique globally?? => bad names
---------------------------------------------

data TParam = Param {
    pnam :: String,
    pval :: String
} deriving (Show)

data TApplyTpl = ApplyTpl {
    tpl    :: String,
    output :: Maybe String,
    ovar   :: Maybe String,
    params :: [TParam]
} deriving (Show)

data TCopy = Copy {
    src :: String,
    dst :: String
} deriving (Show)

data TSave = Save {
    svvar  :: String,
    svfile :: String
} deriving (Show)

data TSelect = Select {
    sfile  :: String,
    sxpath :: String,
    svar   :: String
} deriving (Show)

data TMatchChild = MatchApplyTpl TApplyTpl | MatchCopy TCopy | MatchSave TSave
  deriving (Show)

data TMatch = Match {
    mfile  :: String,
    mxpath :: Maybe String,
    mitems :: [TMatchChild]
} deriving (Show)

data TCreateChild = CreateSelect TSelect | CreateApplyTpl TApplyTpl
  deriving (Show)

data TCreate = Create {
    cfile  :: Maybe String,
    citems :: [TCreateChild]
} deriving (Show)

data TConfigChild = ConfigMatch TMatch | ConfigCreate TCreate
  deriving (Show)

data TConfig = Config {
    directives :: [TConfigChild]
} deriving (Show)

indent :: [String] -> [String]
indent = map ("  "++)

showMaybeStr :: Maybe String -> String
showMaybeStr Nothing  = "-"
showMaybeStr (Just s) = s

showApplyTpl :: TApplyTpl -> [String]
showApplyTpl a = ["Apply-Template: " ++ (tpl a) ++ " -> file: " ++ (showMaybeStr $ output a) ++ " var: " ++ (showMaybeStr $ ovar a)]

showCopy :: TCopy -> String
showCopy c = "Copy: " ++ (src c) ++ " -> " ++ (dst c)

showSave :: TSave -> String
showSave c = "Save: " ++ (svvar c) ++ " -> " ++ (svfile c)

showSelect :: TSelect -> String
showSelect s = "Select: file: " ++ (sfile s) ++ " xpath: " ++ (sxpath s) ++ " var: " ++ (svar s)

showMatchChild :: TMatchChild -> [String]
showMatchChild (MatchApplyTpl a) = showApplyTpl a
showMatchChild (MatchCopy c) = [showCopy c]
showMatchChild (MatchSave s) = [showSave s]

showMatch :: TMatch -> [String]
showMatch m =
    ("Match file: " ++ (mfile m) ++ " xpath: " ++ (showMaybeStr $ mxpath m)) : (indent . concat $ map showMatchChild (mitems m))

showCreateChild :: TCreateChild -> [String]
showCreateChild (CreateApplyTpl a) = showApplyTpl a
showCreateChild (CreateSelect s) = [showSelect s]

showCreate :: TCreate -> [String]
showCreate c =
    ("Create file: " ++ (showMaybeStr $ cfile c)) : (indent . concat $ map showCreateChild (citems c))

showCfgChild :: TConfigChild -> [String]
showCfgChild (ConfigMatch  m) = showMatch m
showCfgChild (ConfigCreate c) = showCreate c

showConfig :: TConfig -> String
showConfig c =
    case directives c of
      []    -> "Config is empty!"
      (h:t) -> unlines . indent . concat $ map showCfgChild (h:t)

---------------------------------------------
-- parser + main program
-- I/O should be done in this section only!
-- TODO parsing without IO (slurp XML first)
---------------------------------------------

parseMaybeAttr :: (ArrowXml a) => String -> a XmlTree (Maybe String)
parseMaybeAttr s = withDefault (getAttrValue0 s >>^ Just) Nothing

-- parse create element (return a singleton list)
parseCreate :: (ArrowXml a) => a XmlTree TCreate
parseCreate =
    (parseMaybeAttr "file") &&& ((getChildren >>> isElem >>>
        ((hasName "select" >>> parseSelect >>^ CreateSelect)
        <+>
        (hasName "apply-template" >>> parseApplyTemplate >>^ CreateApplyTpl))) >. id)
    >>^
    (\(x,y) -> Create {cfile=x, citems=y})

-- parse match element (return a singleton list)
parseMatch :: (ArrowXml a) => a XmlTree TMatch
parseMatch =
    (parseMaybeAttr "xpath") &&& (getAttrValue0 "file")
        &&&
        ((getChildren >>> isElem >>>
          ((hasName "apply-template" >>> parseApplyTemplate >>^ MatchApplyTpl)
            <+>
           (hasName "save" >>> parseSave >>^ MatchSave)
            <+>
           (hasName "copy" >>> parseCopy >>^ MatchCopy))) >. id)
    >>^
    (\(x,(y,z)) -> Match {mfile=y, mxpath=x, mitems=z})

parseSave :: (ArrowXml a) => a XmlTree TSave
parseSave =
    (getAttrValue0 "src") &&& (getAttrValue0 "file")
    >>^
    (\(x,y) -> Save {svvar=x, svfile=y})

parseCopy :: (ArrowXml a) => a XmlTree TCopy
parseCopy =
    (getAttrValue0 "src") &&& (getAttrValue0 "dst")
    >>^
    (\(x,y) -> Copy {src=x, dst=y})

parseSelect :: (ArrowXml a) => a XmlTree TSelect
parseSelect =
    (getAttrValue0 "file") &&& (getAttrValue0 "xpath") &&& (getAttrValue0 "var")
    >>^
    (\(x,(y,z)) -> Select {sfile=x, sxpath=y, svar=z})

-- parse apply-template element (return a singleton list)
parseApplyTemplate :: (ArrowXml a) => a XmlTree TApplyTpl
parseApplyTemplate =
    ((hasAttr "output") <+> (hasAttr "var")) >>. (take 1)
    >>>
    ((parseMaybeAttr "output") &&& (parseMaybeAttr "var") &&& (getAttrValue0 "template")
     &&&
     ((getChildren >>> isElem >>> hasName "param"
      >>>
      ((getAttrValue0 "name") &&& (getAttrValue0 "value"))
      >>^
      (\(x,y) -> Param {pnam=x, pval=y})) >. id))
    >>^
    (\(u,(w,(x,z))) -> ApplyTpl {tpl=x, output=u, ovar=w, params=z})

parseConfig :: (ArrowXml a) => a XmlTree TConfig
parseConfig =
    getChildren >>> isElem >>> hasName "xtiles-config" >>> ((getChildren >>> isElem
    >>>
    ((hasName "match" >>> parseMatch >>^ ConfigMatch)
      <+>   
     (hasName "create" >>> parseCreate >>^ ConfigCreate)))
    >. id)
    >>^
    (\x -> Config {directives=x})

-- parse config from XML String
parseConfigXML :: String -> TConfig
parseConfigXML cfgStr =
    let cfg = runLA (xreadDoc >>> parseConfig) cfgStr
    in
        case cfg of
            [h] -> h
            _ -> error "Config error"

--    cfg <- runX (readDocument [withValidate no] cfgFile
--                 >>>
--                 parseConfig)
--    cfg <- xreadDoc cfgFile >>> parseConfig

--    xmlDoc <- readDocument [withValidate no] cfgFile
--    cfg <- parseConfig xmlDoc

--    putStrLnV $ "got config: " ++ show cfg
--    putStrLnV ""

    --putStrV $ showConfig (cfg!!0)
    --putStrV $ showConfig (cfg!!1)

--    case cfg of
--        [h] -> do 
--            putStrV $ showConfig h
--            return h
--        _ -> error ("Config file format error: " ++ cfgFile)

iov :: (String -> IO ()) -> String -> IO ()
iov f s =
    if verbose then
        f s
    else
        return ()

putStrV :: String -> IO ()
putStrV = iov putStr

putStrLnV :: String -> IO ()
putStrLnV = iov putStrLn

main :: IO ()
main = do
    prgName <- Env.getProgName
    prgArgs <- Env.getArgs
    let cfgFile = case prgArgs of
                [s] -> s :: String
                _   -> error ("usage: " ++ prgName ++ " <config file>")

    putStrLnV $ "Using config file: " ++ cfgFile
    cfgXml <- readFile cfgFile
    putStrLnV $ "config file contents: " ++ cfgXml
    let cfg = parseConfigXML cfgXml
    putStrLnV $ "config: " ++ showConfig cfg
    putStrLnV $ "Program end."

