module Main where

import Graphics.PDF

main :: IO ()
main = do
    let rect = PDFRect 0 0 420 595
    runPdf "blank.pdf" standardDocInfo rect $ addPage Nothing
