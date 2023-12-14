#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [])"

import Data.List
import Data.Bifunctor
import qualified Data.Map as M

pretty :: [String] -> String
pretty = unlines

main = do
  inp <- lines <$> readFile "input.txt"
  print $ task1 inp
  print $ task2 inp

west, east, north, south :: [String] -> [String]
west = map left
east = map (reverse . left . reverse)
north = transpose . west . transpose
south = transpose . east . transpose

left = flip go 0
  where go ('.':xs) n = go xs $ n + 1
        go ('O':xs) n = 'O' : go xs n
        go ('#':xs) n = replicate n '.' ++ '#' : go xs 0
        go [] n = replicate n '.'

task1 inp = weight $ north inp

weight xs =
  sum $ [i * length (filter (== 'O') x) | (x, i) <- zip xs [len, len - 1.. 0]]
  where len = length xs

task2 inp =
  let proc = east . south . west . north
      is = iterate proc inp
      (start, next) = cycles is
  in weight (is !! (start + (1_000_000_000 - start) `rem` (next - start)))

cycles = go 0 mempty
  where go n prev (x:xs) =
          case M.lookup x prev of
            Nothing -> go (n + 1) (M.insert x n prev) xs
            Just i -> (i, n)
