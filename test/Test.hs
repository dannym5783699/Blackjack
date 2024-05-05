
import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn "Arguments passed to the program:"
    mapM_ putStrLn args

