#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [])"

import Data.List (foldl' , transpose)
import Data.Bifunctor (second)
import Debug.Trace (trace)

type Point = (Int, Int)

parseLine :: Int -> [Int] -> String -> Int -> [Point] -> [Point]
parseLine _ _ [] _ acc = acc
parseLine row coles ('#':cs) cc acc =
  parseLine row coles cs (cc + 1) ((row, coles !! cc) : acc)
parseLine row coles ('.':cs) cc acc =
  parseLine row coles cs (cc + 1) acc

parseRows :: [String] -> Int -> Int -> [Int] -> [Point] -> [Point]
parseRows [] _ _ _ acc = acc
parseRows lss@(l:ls) row size coles acc
  | all (== '.') l = parseRows ls (row + size) size coles acc
  | otherwise = parseRows ls (row + 1) size coles $ parseLine row coles l 0 acc

parse :: [String] -> Int -> [Point]
parse lss size = parseRows lss 0 size coles []
  where
    coles = scanl (\acc x -> if all (=='.') x then size + acc
                             else 1 + acc)
            0 (transpose lss)

solve :: [Point] -> Int
solve points = sum [dist x y | x <- points, y <- points, x < y]

dist :: Point -> Point -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

main = do
  input <- lines <$> readFile "input.txt"
  let points = parse input 2
  print $ solve points
  let points = parse input 1000000
  print $ solve points
