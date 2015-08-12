module Main (main) where
import Scheme.Parser as Parser
import Scheme.Eval as Eval
import Scheme.Ast as Ast
import System.Environment
import Text.ParserCombinators.Parsec    
import Control.Monad.Error

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> throwError $ Parser err
                   Right val -> return val
                       
    
