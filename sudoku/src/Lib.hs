module Lib
    ( someFunc
    ) where

import System.IO
import Data.Char
import Data.List
import Control.Monad

someFunc :: IO ()
someFunc = do
  file <- readFile "sudoku.txt"
  -- mapM_ print $ head $ solver $ map toInt $ splitEV 9 $ concat $ lines $ zenToHan $ zenSpaceToHanSpace file  -- ← 全部の問題を再帰して解いてくれると思ったけどダメだった。
  -- mapM_ print $ head $ solver $ map toInt $ splitEV 9 $ fst $ span(/='\n') $ zenToHan $ zenSpaceToHanSpace file　-- ← 一列取得して、数独解いて出力できる。
  -- mapM_ print $ head $ solver $ map toInt $ splitEV 9 $ fst $ span(/='\n') $ zenToHan $ zenSpaceToHanSpace file
  -- mapM_ print $ map head $ getLow $ span(/='\n') $ zenToHan $ zenSpaceToHanSpace file
  mapM_ print $ getLow $ span(/='\n') $ zenToHan $ zenSpaceToHanSpace file
  -- print $ splitTE 9 $ splitEV 9 $ toInt $ concat $ lines $ zenToHan $ zenSpaceToHanSpace file　← [[[list]]]（リストのリストのリスト）を作成しようと思った。
  -- mapM_ print $ map toInt $ splitEV 9 $ concat $ lines $ zenToHan $ zenSpaceToHanSpace file　← Int型にしたfileを出力できる。

---- spanしたタプル(↓の形)をうけとってresolverに渡してそれ以外はgetLowに返し再帰させる。そうするとタプルのfst(左側)がxに入り、resolverもループ的な処理が実行可能になる。---
getLow :: ([Char], [Char]) ->[Board]
getLow ([],[]) = []
getLow (x,xs) = resolver x : getLow(span(/='\n') $ tail xs)

-- loopHead :: [Board] -> [Board]
-- loopHead [] = []
-- loopHead (x:xs) =  tail x : xs

-- タプルを受け取ってどういう形で出力されるか確認したかったので作った関数
-- getLow :: ([Char], [Char]) ->[[Char]]
-- getLow ([],[]) = []
-- getLow (x,xs) = x : getLow(span(/='\n') $ tail xs)
------------------------------------------------------------------------------------------------------------------------------------------------------

----- solverを呼び出して問題を解いてもらう -----------------------------------------------------------
resolver :: String -> Board
resolver list = head $ solver $ map toInt $ splitEV 9 list

-- 先代まとめてぶち込んだらできるかな安直に考えたがダメだった ↓
-- resolver :: ([Char], [Char]) -> Board
-- resolver ([],[]) = []
-- resolver (x,xs) = head $ solver $ map toInt $ splitEV 9 x : resolver $ span(/='\n') xs
--------------------------------------------------------------------------------------------------

-- もらった変数を::の後の型に置き換える(使えなかった。20以上の文字はバグる) ------
readNum :: String -> Int
readNum list = read list :: Int
---------------------------------------------------------------------------

-- もらった変数を::の後の型に置き換えるそれをマップする ----------
toInt :: [Char] -> [Int]
toInt str = map (read . return) str :: [Int]
----------------------------------------------------------

-- 全角から半角に変える ----------------------------------------
toHanChar :: Char -> Char
-- 全角文字からシフト分の文字コード(0xfee0)を引いてあげると半角になる
toHanChar c = chr (ord c - 0xfee0)

zenToHan :: String -> String
zenToHan []     = []
zenToHan (c:cs) = if ord c >= 0xff01 && ord c <= 0xff5e 
                      then toHanChar c : zenToHan cs
                      else c:zenToHan cs
-------------------------------------------------------------

-- スペースの置換 ---------------------------------------------
zenSpaceToHanSpace :: String -> String
zenSpaceToHanSpace []     = []
zenSpaceToHanSpace (c:cs) = if c == '\12288'
                      then '0' : zenSpaceToHanSpace cs
                      else c : zenSpaceToHanSpace cs
-------------------------------------------------------------

-- 引数の数値の数で分割　-------------------------------------
splitEV :: Int -> [a] -> [[a]]
splitEV  _ [] = []
-- takeで引数分とったら残りはdropして再帰に回す。
splitEV n list = take n list:(splitEV n $ drop n list)
-----------------------------------------------------------

-- splitTE :: Int -> [[a]] -> [[[a]]]
-- splitTE  _ [] = []
-- splitTE n list = take n list:(splitTE n $ drop n list)

-- 盤面
type Board = [[Int]]

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

