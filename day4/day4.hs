#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

import Text.Parsec
import Data.Char (isDigit)
import Data.Maybe
import Data.Either (fromRight)
import Debug.Trace
import qualified Data.Set as S
import Data.Function (on)

type Parser a = Parsec String () a

type Game = ([Int], [Int])

parseLine :: Parser Game
parseLine = do
  string "Card" <* many space
  many1 digit
  string ":" <* many space
  winNums <- many1 digit `sepEndBy1` many1 (char ' ')
  string "|" <* many1 (char ' ')
  myNums <- many1 digit `sepEndBy1` many1 (char ' ')
  return (map read winNums, map read myNums)

parseFile :: String -> [Game]
parseFile str = case parse (many (parseLine <* newline) <* eof) "" str of
                  Left e -> error $ show e
                  Right x -> x

(...) = (.).(.)

numWins :: [Int] -> [Int] -> Int
numWins = S.size ... S.intersection `on` S.fromList

solve1 :: [Int] -> [Int] -> Int
solve1 win my = if size == 0 then 0 else 2 ^ (size - 1)
  where size = numWins win my

solve2 :: [([Int], [Int])] -> [Int]
solve2 [] = []
solve2 (x:xs) = 1 + sum (take size rest) : rest
  where (win, my) = x
        size = numWins win my
        rest = solve2 xs


task1 = print . sum . map (uncurry solve1) . parseFile
task2 = print . sum . solve2 . parseFile


main = do
  input <- readFile "input.txt"
  task1 input
  task2 input
