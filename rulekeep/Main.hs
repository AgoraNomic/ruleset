module Main where

import Data.List
import Data.Yaml
import Control.Monad
import System.Directory
import System.FilePath
import System.IO

import Game.Agora.Data.Rule

main :: IO ()
main = do
    let ruleDir = "rules"
    files <- fmap (map (ruleDir </>)) $ listDirectory ruleDir
    rules <- mapM decodeFileEither files
    mapM_ (uncurry printStatus) $ zip files rules
  where
    printStatus :: FilePath -> Either ParseException Rule -> IO ()
    printStatus f (Left e) = putStrLn $ f ++ ": " ++ prettyPrintParseException e
    printStatus f (Right r) = putStrLn $ "Parsed " ++ f ++ ": " ++ (show $ name r)
