import System.Environment

import Contract

main = do
    args <- getArgs
    putStrLn $ show $ score (read (head args))
