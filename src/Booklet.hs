module Booklet where

pageStrFunc :: Int -> Int -> String
pageStrFunc pageCount page =
    if pageCount < page then "B1" else "A" ++ (show page)

pageOrder :: Int -> [Int]
pageOrder c = _pages 1 $ bookPageCount c
    where
        _pages a b =
            if a < b
            then [b, a, a + 1, b - 1] ++ _pages (a + 2) (b - 2)
            else []

bookPageCount :: Int -> Int
bookPageCount c =
    if mod c 4 == 0
    then c
    else 4 * (1 + div c 4)
