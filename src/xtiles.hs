import qualified Data.Map as Map (Map)
import qualified System.Environment as Env (getArgs, getProgName)
import           Text.XML.HXT.Core

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
--
-- TODO nasty things (does it have to be like this?):
-- 1. lack of type unions (lots of nested constructors instead)
-- 2. record field names must be unique globally?? => bad names
---------------------------------------------

data TApplyTpl = ApplyTpl {
    tpl :: String,
    output :: String
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

-- TODO nasty! does it have to be like this?
data TMatchChild = MatchApplyTpl TApplyTpl | MatchCopy TCopy
  deriving (Show)

data TMatch = Match {
    mfile  :: String,
    mxpath :: String,
    mitems :: [TMatchChild]
} deriving (Show)

-- TODO nasty! does it have to be like this?
data TCreateChild = CreateSelect TSelect | CreateApplyTpl TApplyTpl
  deriving (Show)

data TCreate = Create {
    cfile  :: String,
    citems :: [TCreateChild]
} deriving (Show)

---------------------------------------------
-- parser + main program
-- I/O should be done in this section only!
---------------------------------------------

parseMatch :: IOSArrow XmlTree TMatch
parseMatch =
    getChildren
    >>>
    isElem >>> hasName "apply-template"
    >>>
    parseApplyTemplate
    >>>
    arr (\x -> Match { mfile="", mxpath="", mitems=[MatchApplyTpl x]})

parseApplyTemplate :: IOSArrow XmlTree TApplyTpl
parseApplyTemplate =
    (getAttrValue "template") &&& (getAttrValue "output")
    >>>
    arr (\(x,y) -> ApplyTpl{tpl=x, output=y})

parseConfigXML :: IOSArrow XmlTree TMatch
parseConfigXML =
    getChildren
    >>>
    isElem >>> hasName "xtiles-config"
    >>>
    getChildren
    >>>
    isElem >>> hasName "match"
    >>>
    parseMatch

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

    _ <- runX ( readDocument [withValidate no
                        --,withCurl []
                        ] cfgFile
           >>>
           writeDocument [withIndent yes
                         ,withOutputEncoding utf8
                         ] "-")

    putStrLnV $ "Program end."
