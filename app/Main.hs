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
renamePath path = reassemble $ path =~ ("\\\\MAD\\\\" :: String)
  where
    reassemble :: (String, String, String) -> String
    reassemble (p, "", _) = p
    reassemble (pre, _, post) = pre <> "\\MVP\\" <> post

replaceLines :: T.Text -> T.Text
replaceLines = T.unlines . map (replacePrinterLine . replaceCountLine) . T.lines

replacePrinterLine :: T.Text -> T.Text
replacePrinterLine line = substitutePrinter $ line =~ (T.pack "^PRINTER ")
  where
    substitutePrinter :: (T.Text, T.Text, T.Text) -> T.Text
    substitutePrinter (p, "", "") = p
    substitutePrinter _ = "PRINTER \\\\MVPSRV-APP01\\PRT-ETQ-IT"

replaceCountLine :: T.Text -> T.Text
replaceCountLine line = substituteCount $ line =~ (T.pack "^PRINT [0-9]+$")
  where
    substituteCount :: (T.Text, T.Text, T.Text) -> T.Text
    substituteCount (p, "", "") = p
    substituteCount _ = "PRINT 1"
