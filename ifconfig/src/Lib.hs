module Lib
    ( someFunc
    ) where

import System.Process
import System.IO
import Data.Char

someFunc :: IO ()
someFunc = do
  h <- getStdOut
  -- print $ span (/= ':') h
  putStrLn $ addHeadder(lines h)"" -- linesで１行ずつ取得 

getStdOut :: IO String
getStdOut = do
  (_, Just out, _, _) <-createProcess (proc "ifconfig" []){ std_out = CreatePipe } -- コマンドの実行"ifconfig"
  ss <- hGetContents out -- hGetContents実行の全文取得
  return ss

addHeadder :: [String] -> String -> String
addHeadder []_ = ""
addHeadder(l:ls) h
  | l == "" = '\n' : (body "") -- 空行判定
  | isLetter (head l) = '\n' : l ++ body(header) -- 先頭に文字（ヘッダー）がある行の場合
  | otherwise = '\n' : h ++ l ++ body(header) -- 上記に該当しない場合、つまり、空行でもヘッダーもない行の場合
  where
    header = if isLetter(head l) then (fst $ span (/=':') l) ++ ":" else h -- 文字が最初にあった場合はspanんで区切った左側（fst）を取得
    body = addHeadder(ls) --　再帰処理をするため自分を呼び出してる。lsを指定することで判定したとこ以外の列に対して再起処理を行う。

