#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"
import Debug.Trace
import Data.List
import Data.Function

parse = map (tail . words) . lines

f :: Int -> Int -> Int -> Int
f dist time x = if x * (time - x) > dist then 1 else 0

task1 :: [(Int, Int)] -> Int
task1 =
  product . map (\(time, dist) -> foldl' (\acc x -> f dist time x + acc) 0 [0..time])

task2 :: Int -> Int -> Int
task2 time dist =
  foldl' (\acc x -> f dist time x + acc) 0 [0..time]

main = do
  input <- readFile "input.txt"
  let [times,dists] = parse input
  print $ task1 $ zip (map read times) (map read dists)
  print (on task2 (read . intercalate "") times dists)
