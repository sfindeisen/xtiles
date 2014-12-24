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

data TParamValue = TString (Maybe String) | TFile (Maybe String) | TInteger (Maybe Integer) deriving (Show)
data TParam = Param {
    name     :: String,
    value    :: TParamValue
} deriving (Show)

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
