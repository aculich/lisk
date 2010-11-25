module Main where

import Control.Monad
import Language.Lisk.Parser
import System.Environment
import System.Exit
import System.IO
import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Regex

main = do
  (original:input:output:_) <- getArgs
  str <- readFile input
  case parseLisk original (unlines . ("":) . drop 1 . lines $ str) of
    Left err  -> do hPutStr stderr $ showError err
                    exitWith $ ExitFailure 1
    Right src -> writeFile output $ printLiskToHaskell src
  where showError err = line ++ msg ++ suggest where
            line = format . unlines . take 1 . lines $ e
            msg = (unlines . drop 1 . lines $ e)
            e = show err
            
format = flip (subRegex (mkRegex r)) "\\1:\\2:\\3:" where
  r = "^\"([^\"]+)\" \\(line ([0-9]+), column ([0-9]+)\\):$"


bothOn f g x y = f (g x y) (g y x)
