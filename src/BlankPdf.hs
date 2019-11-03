module BlankPdf where

writeBlankPdf :: Int -> Int -> IO String
writeBlankPdf width height = do
  let filename = "blank-" ++ (show width) ++ "-" ++ (show height) ++ ".pdf"
  writeFile filename $ blank width height
  return filename


blank :: Int -> Int -> String
blank width height =
  let
    wstr = show width
    hstr = show height
    strlen = length $ wstr ++ hstr
    mediabox = "/MediaBox [0 0 " ++ wstr ++ " " ++ hstr ++ "]"
  in unlines [ "%PDF-1.1"
  , "1 0 obj <</Type /Catalog /Pages 2 0 R>> endobj"
  , "2 0 obj <</Type /Pages /Kids [3 0 R] /Count 1>> endobj"
  , "3 0 obj <</Type /Page /Parent 2 0 R" ++ mediabox ++ ">> endobj"
  , "xref 0 5"
  , "0000000000 65535 f "
  , "0000000009 00000 n "
  , "0000000056 00000 n "
  , "0000000111 00000 n "
  , "trailer <</Root 1 0 R /Size 5>>"
  , "startxref " ++ show (175 + strlen)
  , "%%EOF"
  ]
