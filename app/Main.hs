module Main where

import System.Environment

import BlankPdf
import Booklet
import File
import Pdftk

main :: IO ()
main = do
    args <- getArgs
    info <- mapM pdfInfo args
    booklets <- mapM makeBooklet $ filter bookletCandidate info
    mapM_ putStrLn booklets


bookletCandidate :: PDFInfo -> Bool
bookletCandidate i = case i of
    Invalid -> False
    PDFInfo _ c _ _ -> if c < 3 then False else True

makeBooklet :: PDFInfo -> IO String
makeBooklet Invalid = do return "invalid" -- should never get here
makeBooklet (PDFInfo filepath count width height) = do
    blank <- writeBlankPdf (round width) (round height)
    booklet <- writeBookletPdf filepath blank count
    remove blank
    return booklet

writeBookletPdf :: String -> String -> Int -> IO String
writeBookletPdf inputPath blank count = do
    let pages = map (pageStrFunc count) (pageOrder count)
        outputPath = "booklet-" ++ inputPath
    pdfCat [inputPath, blank] pages outputPath
