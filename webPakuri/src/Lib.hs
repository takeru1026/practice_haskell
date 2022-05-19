module Lib
    ( someFunc
    ) where

import Data.List
import Control.Monad

someFunc :: IO ()
someFunc = mapM_ print $ head $ solver q00

-- 盤面
type Board = [[Int]]

-- 数字を取り出す
getNum :: Board -> Int -> Int -> Int
getNum board x y = (board !! y) !! x

-- 列を取り出す
getColumn :: Board -> Int -> [Int]
getColumn board n = map head $ map (drop n) board

-- (x,y) の枠を取り出す
getGroup :: Board -> Int -> Int -> [Int]
getGroup board x y = concatMap (f x') $ f y' board
  where x' = x `div` 3
        y' = y `div` 3
        f n xs = take 3 $ drop (n * 3) xs

-- n 番目の要素を m に置き換える
substNth :: [a] -> Int -> a -> [a]
substNth []     _ _ = []
substNth (x:xs) 0 m = m : xs
substNth (x:xs) n m = x : substNth xs (n - 1) m

-- 数字を書き込む
putNum :: Board -> Int -> Int -> Int -> Board
putNum board x y n =
  substNth board y $ substNth (board !! y) x n

-- 解法
solver :: Board -> [Board]
solver board = iter board makeIdx
  where makeIdx = [(x,y) | y <- [0..8], x <- [0..8], getNum board x y == 0]
        iter board [] = return board
        iter board ((x, y):idx) = do
          let xs = getColumn board x
              ys = board !! y
              gs = getGroup board x y
          n <- [1..9]
          guard(n `notElem` xs)
          guard(n `notElem` ys)
          guard(n `notElem` gs)
          iter (putNum board x y n) idx


q00 :: Board
q00 = [[9,0,0,0,8,0,0,0,5],[0,7,0,0,0,9,0,4,0],[0,0,6,0,0,3,2,0,0],[0,2,1,0,0,0,0,0,0],[8,0,0,0,0,0,0,0,7],[0,0,0,0,0,0,5,9,0],[0,0,4,7,0,0,3,0,0],[0,8,0,6,0,0,0,1,0],[2,0,0,0,5,0,0,0,8]]