{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  filesIn <- getArgs
  mapM_ processFile filesIn

processFile :: FilePath -> IO ()
processFile fileIn = do
  content <- TIO.readFile fileIn
  let output = replaceLines content
  let fileOut = renamePath fileIn
  TIO.writeFile fileOut output

renamePath :: FilePath -> FilePath
renamePath path = reassemble $ path =~ ("\\\\(MAD|MVQ|MVP)\\\\" :: String)
  where
    reassemble :: (String, String, String) -> String
    reassemble (p, "", _) = p
    reassemble (pre, _, post) = pre <> "\\MVP\\" <> post

replaceLines :: T.Text -> T.Text
replaceLines = T.unlines 
  . map (
      replaceLine "^PRINTER " "PRINTER \\\\MVPSRV-APP01\\PRT-ETQ-IT"
      . replaceLine "^PRINT [0-9]+$" "PRINT 1"
  ) . T.lines

replaceLine :: T.Text -> T.Text -> T.Text -> T.Text
replaceLine match replacement line = replaceIfMatch $ line =~ match
  where
    replaceIfMatch :: (T.Text, T.Text, T.Text) -> T.Text
    replaceIfMatch (p, "", "") = p
    replaceIfMatch _ = replacement
