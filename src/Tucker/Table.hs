module Tucker.Table where

import Data.List

import Tucker.Util

data TableConf =
    TableConf {
        col_gap :: Int
    }

instance Default TableConf where
    def =
        TableConf {
            col_gap = 3
        }

fillCols :: [[a]] -> a -> [[a]]
fillCols cols def =
    let max_len = maximum (map length cols) in
    for cols $ \col ->
        col ++ replicate (max_len - length col) def

-- generate a table
table :: TableConf -> [[String]] -> String
table conf cols' =
    let cols = fillCols cols' ""
        max_len = maximum (map length cols)
        preproc =
            for (zip [0..] cols) $ \(i, col) ->
                let lens = map length col
                    width = if i == length cols - 1 then 0
                            else maximum lens + col_gap conf
                in for col $ \elem ->
                    elem ++ replicate (width - length elem) ' '
    in
        unlines $
        foldl (\l0 l -> map (\(e0, e) -> e0 ++ e) (zip l0 l))
              (replicate max_len "") preproc
