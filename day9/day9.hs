#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [])"

solve :: ([Int] -> Int -> Int) -> [Int] -> Int
solve f xs = if all (== 0) xs then 0 else f xs $ solve f $ zipWith (flip (-)) xs (tail xs)

task1 = solve ((+) . last)

task2 = solve ((-) . head)

main = do
  input <- readFile "input.txt"
  let res f = (print . sum) (map (f . map read . words) (lines input))
  res task1
  res task2
