module Main where
import Parser
import Eval


main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> val
                       
    
