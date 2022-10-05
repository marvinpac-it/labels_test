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
  let output = replacePrinter content
  let fileOut = renamePath fileIn
  TIO.writeFile fileOut output

replacePrinter :: T.Text -> T.Text
replacePrinter = T.unlines . replacePrinter' . T.lines

replacePrinter' :: [T.Text] -> [T.Text]
replacePrinter' [] = []
replacePrinter' (x:xs) = (replacePrinterLine x) : (replacePrinter' xs)

renamePath :: FilePath -> FilePath
renamePath path = reassemble $ path =~ ("\\\\MAD\\\\" :: String)
  where
    reassemble :: (String, String, String) -> String
    reassemble (p, "", _) = p
    reassemble (pre, _, post) = pre <> "\\MVP\\" <> post

replacePrinterLine :: T.Text -> T.Text
replacePrinterLine line = substitute $ line =~ (T.pack "^PRINTER ")
  where
    substitute :: (T.Text, T.Text, T.Text) -> T.Text
    substitute (p, "", "") = p
    substitute _ = "PRINTER \\\\MVPSRV-APP01\\PRT-ETQ-IT"
