import qualified System.Environment as Env (getArgs, getProgName)

---------------------------------------------
-- constants
---------------------------------------------

verbose :: Bool
verbose = True
   
---------------------------------------------
-- xtiles
---------------------------------------------

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
