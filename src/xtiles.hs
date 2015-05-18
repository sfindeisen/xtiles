import qualified System.Environment as Env (getArgs, getProgName)
import           Config

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
-- parser + main program
-- I/O should be done in this section only!
-- TODO parsing without IO (slurp XML first)
---------------------------------------------

iov :: (String -> IO ()) -> String -> IO ()
iov f s =
    if verbose then
        f s
    else
        return ()

-- putStrV :: String -> IO ()
-- putStrV = iov putStr

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
    -- putStrLnV $ "config file: " ++ cfgXml
    let cfg = parseConfigXML cfgXml
    putStrLnV $ "config: " ++ showConfig cfg
    putStrLnV $ "Program end."

