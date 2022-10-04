{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Monoid ((<>))
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  fileIn <- fmap (T.pack . head) getArgs
  content <- TIO.readFile (T.unpack fileIn)
  let output = replacePrinter content
  -- let (pre, _, post) = fileIn =~ "/MAD/"
  -- let fileOut = pre <> "/MVP/" <> post
  let fileOut = renamePath fileIn
  TIO.writeFile (T.unpack fileOut) output

replacePrinter :: T.Text -> T.Text
replacePrinter = T.unlines . replacePrinter' . T.lines

replacePrinter' [] = []
replacePrinter' (x:xs) = (replacePrinterLine x) : (replacePrinter' xs)

renamePath :: T.Text -> T.Text
renamePath path = reassemble $ path =~ (T.pack "\\\\MAD\\\\")
  where
    reassemble :: (T.Text, T.Text, T.Text) -> T.Text
    reassemble (p, "", _) = p
    reassemble (pre, _, post) = pre <> (T.pack "\\MVP\\") <> post

replacePrinterLine :: T.Text -> T.Text
replacePrinterLine line = substitute $ line =~ (T.pack "^PRINTER ")
  where
    substitute :: (T.Text, T.Text, T.Text) -> T.Text
    substitute (p, "", "") = p
    substitute _ = "PRINTER \\\\MVPSRV-APP01\\PRT-ETQ-IT"
