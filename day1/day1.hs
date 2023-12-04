#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

import Data.Maybe
import Data.List
import Data.Char (isDigit)
import Text.Parsec
import Control.Monad (void)

type Parser a = Parsec String () a

parseDigit :: String -> Maybe Char
parseDigit [] = Nothing
parseDigit (x:xs)
  | isDigit x = Just x
  | otherwise = Nothing

parseWord :: String -> Maybe Char
parseWord s
  | "one" `isPrefixOf` s = Just '1'
  | "two" `isPrefixOf` s = Just '2'
  | "three" `isPrefixOf` s = Just '3'
  | "four" `isPrefixOf` s = Just '4'
  | "five" `isPrefixOf` s = Just '5'
  | "six" `isPrefixOf` s = Just '6'
  | "seven" `isPrefixOf` s = Just '7'
  | "eight" `isPrefixOf` s = Just '8'
  | "nine" `isPrefixOf` s = Just '9'
  | otherwise = parseDigit s

parseLine :: (String -> Maybe Char) -> String -> Int
parseLine p s = read [head ds, last ds]
  where ds = mapMaybe p $ tails s

solve :: (String -> Int) -> String -> Int
solve f s = sum $ map f $ lines s
-- helper :: String -> Int
task1 = do
  input <- readFile "input.txt"
  print $ solve (parseLine parseDigit) input

task2 = do
  input <- readFile "input.txt"
  print $ solve (parseLine parseWord) input


main = do
  task1
  task2
