import qualified Data.Map as Map (Map)
import qualified System.Environment as Env (getArgs, getProgName)
import           Text.XML.HXT.Core
import           Control.Arrow.ArrowList

---------------------------------------------
-- constants
---------------------------------------------

verbose :: Bool
verbose = True

---------------------------------------------
-- expressions
---------------------------------------------

data TLiteral    = LString String | LInteger Integer | LBool Bool deriving (Eq, Show)
data TIdent      = Ident String
data TFunCall    = FunCall { funcName :: String, funcParams :: [TExpr] } deriving (Show)
data TExpr       = TLiteral | TIdent | TFunCall | CmpEq TExpr TExpr deriving (Show)

-- type expressions are used with const/param declarations; TODO: elem list? attr list? nodeset?
data TTypeExpr   = UTypeString | UTypeInteger | UTypeBool | UTypeFile | UTypeXPath deriving (Eq, Show)

---------------------------------------------
-- runtime
---------------------------------------------

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

---------------------------------------------
-- types
-- TODO nasty things (does it have to be like this?):
-- 1. lack of type unions (lots of nested constructors instead)
-- 2. record field names must be unique globally?? => bad names
---------------------------------------------

data TApplyTpl = ApplyTpl {
    tpl    :: String,
    output :: Maybe String,
    ovar   :: Maybe String
} deriving (Show)

data TCopy = Copy {
    src :: String,
    dst :: String
} deriving (Show)

data TSelect = Select {
    sfile  :: String,
    sxpath :: String,
    svar   :: String
} deriving (Show)

data TMatchChild = MatchApplyTpl TApplyTpl | MatchCopy TCopy
  deriving (Show)

data TMatch = Match {
    mfile  :: String,
    mxpath :: Maybe String,
    mitems :: [TMatchChild]
} deriving (Show)

data TCreateChild = CreateSelect TSelect | CreateApplyTpl TApplyTpl
  deriving (Show)

data TCreate = Create {
    cfile  :: String,
    citems :: [TCreateChild]
} deriving (Show)

data TConfigChild = ConfigMatch TMatch | ConfigCreate TCreate
  deriving (Show)

data TConfig = Config {
    directives :: [TConfigChild]
} deriving (Show)

showMaybeStr :: Maybe String -> String
showMaybeStr Nothing  = "-"
showMaybeStr (Just s) = s

showMatch :: TMatch -> String
showMatch m = "Match file: " ++ (mfile m) ++ " xpath: " ++ (showMaybeStr $ mxpath m)

showCreate :: TCreate -> String
showCreate _ = "create"

showCfgChild :: TConfigChild -> String
showCfgChild (ConfigMatch  m) = showMatch m
showCfgChild (ConfigCreate c) = showCreate c

showConfig :: TConfig -> String
showConfig c =
    case directives c of
      []    -> "No directive!"
      (h:t) -> unlines $ map showCfgChild (h:t)

---------------------------------------------
-- parser + main program
-- I/O should be done in this section only!
---------------------------------------------

parseMaybeAttr :: String -> IOSArrow XmlTree (Maybe String)
parseMaybeAttr s = withDefault (getAttrValue0 s >>^ Just) Nothing

-- parse match element (return a singleton list)
parseMatch :: IOSArrow XmlTree TMatch
parseMatch =
    (((withDefault (getAttrValue0 "xpath" >>^ Just) Nothing) &&& (getAttrValue0 "file"))
        &&&
        ((getChildren
          >>>
          isElem >>> hasName "apply-template"
          >>>
          parseApplyTemplate >>^ MatchApplyTpl) >. id))
    >>^
    (\((x,y),z) -> Match {mfile=y, mxpath=x, mitems=z})

parseApplyTemplate :: IOSArrow XmlTree TApplyTpl
parseApplyTemplate =
    (hasAttr "output" <+> hasAttr "var")
    >>>
    ((parseMaybeAttr "output") &&& (parseMaybeAttr "var")) &&& (getAttrValue0 "template")
    >>^
    (\((x,y),z) -> ApplyTpl {tpl=z, output=x, ovar=y})

parseConfig :: IOSArrow XmlTree TConfig
parseConfig =
    ((getChildren
    >>>
    isElem >>> hasName "xtiles-config"
    >>>
    getChildren
    >>>
    isElem >>> hasName "match"
    >>>
    parseMatch >>^ ConfigMatch) >. id)
    >>^
    (\x -> Config {directives=x})

parseConfigXML :: String -> IO TConfig
parseConfigXML cfgFile = do
    cfg <- runX (readDocument [ withValidate no ] cfgFile
                 >>>
                 parseConfig)

    case cfg of
        []    -> error ("Unable to read config file: " ++ cfgFile)
        h:_:_ -> error ("Config file format error: " ++ cfgFile)
        (h:_) -> do 
            putStrLnV $ "got config: " ++ show h
            putStrLnV $ showConfig h
            return h

iov :: (String -> IO ()) -> String -> IO ()
iov f s =
    if verbose then
        f s
    else
        return ()

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
    cfg <- parseConfigXML cfgFile
    putStrLnV $ "Program end."
