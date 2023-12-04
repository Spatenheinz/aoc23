#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

import Text.Parsec
import Data.Char (isDigit)
import Data.Maybe
import Data.Either (fromRight)
import Debug.Trace
import qualified Data.Set as S

type Parser a = Parsec String () a

type Game = (Int, S.Set Int, S.Set Int)

parseLine :: Parser Game
parseLine = do
  string "Card" <* many space
  nr <- read <$> many1 digit
  string ":" <* many space
  winNums <- many1 digit `sepEndBy1` many1 (char ' ')
  string "|" <* many1 (char ' ')
  myNums <- many1 digit `sepEndBy1` many1 (char ' ')
  return (nr, S.fromList $ map read winNums, S.fromList $ map read myNums)

parseFile :: String -> [Game]
parseFile str = case parse (many (parseLine <* newline) <* eof) "" str of
                  Left e -> error $ show e
                  Right x -> x

solve1 (_, win, my) = if size == 0 then 0 else 2 ^ (size - 1)
  where size = numWins win my

(...) = (.).(.)

numWins :: S.Set Int -> S.Set Int -> Int
numWins = S.size ... S.intersection

task1 = print . sum . map solve1 . parseFile


main = do
  input <- readFile "input.txt"
  task1 input
