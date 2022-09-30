module Main where

import System.Environment (getArgs)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  fileIn:_ <- getArgs
  content <- B.readFile fileIn
  let output = replacePrinter content
  -- let (pre, _, post) = fileIn =~ "/MAD/"
  -- let fileOut = pre ++ "/MVP/" ++ post
  let fileOut = renamePath fileIn
  putStrLn "test"

replacePrinter :: B.ByteString -> B.ByteString
replacePrinter = B8.unlines . replacePrinter' . B8.lines

replacePrinter' [] = []
replacePrinter' (x:xs) = (replacePrinterLine x) : (replacePrinter' xs)

renamePath :: String -> String
renamePath path = reassemble $ path =~ "/MAD/"
  where
    reassemble (p, "", _) = p
    reassemble (pre, _, post) = pre ++ "/MVP/" ++ post

replacePrinterLine :: B.ByteString -> B.ByteString
replacePrinterLine line = substitute $ line =~ (B.pack "^PRINTER ")
  where
    substitute (p, "", "") = p
    substitute _ = "PRINTER \\\\MVPSRV-APP01\\PRT-ETQ-IT"
