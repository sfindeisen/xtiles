-- import qualified Data.Map as Map (Map)
import qualified System.Environment as Env (getArgs, getProgName)
import Text.XML.HXT.Core

---------------------------------------------
-- constants
---------------------------------------------

verbose :: Bool
verbose = True
   
---------------------------------------------
-- xtiles
---------------------------------------------

data TLiteral    = LString String | LInteger Integer | LBool Bool deriving (Eq, Show)
data TFunCall    = FunCall { funcName :: String, funcParams :: [TExpr] } deriving (Show)
-- data TTplCall    = TplCall {  tplName :: String,  tplParamBindings :: Map.Map String TExpr } deriving (Show)
data TExpr       = TLiteral | Ident String | TFunCall | CmpEq TExpr TExpr deriving (Show)
-- data TType       = TString | TInteger | TBool | TFile | TXPath | TXPathResult deriving (Eq, Show)
-- data TParamDecl  = ParamDecl { pName :: String, pDefaultValue :: Maybe TExpr, pType :: TType }

---------------------------------------------
-- main program
-- I/O should be in this section only!
---------------------------------------------

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
