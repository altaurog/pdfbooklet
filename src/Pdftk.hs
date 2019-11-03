module Pdftk where

import Data.List
import System.Exit
import System.Process

import Safe

data PDFDetail = PageCount Int | Dimensions Float Float deriving (Show)

data PDFInfo = Invalid | PDFInfo String Int Float Float deriving (Show, Eq)

pdfInfo :: String -> IO PDFInfo
pdfInfo filepath = do
    let cmd = proc "pdftk" [filepath, "dump_data"]
    (exitcode, stdout, _) <- readCreateProcessWithExitCode cmd ""
    case exitcode of
        ExitSuccess -> return $ parseInfo filepath stdout
        _ -> return Invalid

pdfCat :: [String] -> [String] -> String -> IO String
pdfCat files pages outputPath = do
    let
        prefixes = map (: "=") ['A'..'Z']
        inputs = map (uncurry (++)) (zip prefixes files)
        args = inputs ++ ["cat"] ++ pages ++ ["output", outputPath]
        cmd = proc "pdftk" args
    (exitcode, _, _) <- readCreateProcessWithExitCode cmd ""
    case exitcode of
        ExitSuccess -> return outputPath
        _ -> return "error"

parseInfo :: String -> String -> PDFInfo
parseInfo filepath info =
    case mapM ($ info) [infoPageCount, infoDimensions] of
        Just [PageCount count, Dimensions width height]
          -> PDFInfo filepath count width height
        _ -> Invalid

infoPageCount :: String -> Maybe PDFDetail
infoPageCount info = do
    detail <- getInfoDetail "NumberOfPages" info
    countStr <- headMay detail
    fmap PageCount $ readMay countStr

infoDimensions :: String -> Maybe PDFDetail
infoDimensions info = do
    detail <- getInfoDetail "PageMediaDimensions" info
    case mapM readMay detail of
        Just [width, height] -> Just (Dimensions width height)
        _ -> Nothing

getInfoDetail :: String -> String -> Maybe [String]
getInfoDetail field info = do
    let match = isPrefixOf (field ++ ": ")
    line <- headMay $ filter match $ lines info
    tailMay $ words line
