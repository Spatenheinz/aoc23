#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [])"

import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.List (find)

type PipeMap = M.Map (Int, Int) Char

convert :: [String] -> PipeMap
convert = M.fromList . concat . zipWith (\r -> zipWith (\c ch -> ((r, c), ch)) [0..]) [0..]

initial :: PipeMap -> (Int, Int)
initial = fst . fromJust . find ((=='S') . snd) . M.toList

solve :: [String] -> Int
solve input = length (loop input) `div` 2

loop :: [String] -> PipeMap
loop input =
  let pipes = convert input
      rows = length input
      cols = length $ head input
      s = initial pipes
  in go pipes (rows, cols) M.empty [s]
  where go :: PipeMap -> (Int, Int) -> PipeMap -> [(Int, Int)] -> PipeMap
        go pipes (rows, cols) seen [] = seen
        go pipes (rows, cols) seen (x@(r, c):rest) =
          let ch = case M.lookup (r, c) pipes of
                     Just x -> x
                     Nothing -> error $ show (r, c, cols, pipes)
              seen' = M.insert (r, c) ch seen
          in if (r, c) `M.member` seen then go pipes (rows, cols) seen' rest
          else
            let ups = ([(r - 1, c) | 0 < r && canFlowUp ch])
                downs = ([(r + 1, c) | r < rows && canFlowDown ch])
                lefts = ([(r, c - 1) | 0 < c && canFlowLeft ch])
                rights = ([(r, c + 1) | c < cols && canFlowRight ch])
            in go pipes (rows, cols) seen' (ups ++ downs ++ lefts ++ rights ++ rest)

simplify :: [String] -> PipeMap -> PipeMap
simplify input lp = M.mapWithKey (\(r,c) ch -> if (r,c) `M.member` lp then ch else '.') $ convert input

solve2 input = simplify input (loop input)

canFlowUp :: Char -> Bool
canFlowUp = (`elem` "SJL|")

canFlowDown :: Char -> Bool
canFlowDown = (`elem` "SF7|")

canFlowLeft :: Char -> Bool
canFlowLeft = (`elem` "S7J-")

canFlowRight :: Char -> Bool
canFlowRight = (`elem` "SFL-")

main = do
  input <- lines <$> readFile "input.txt"
  print $ solve $ input
  print $ solve2 $ input
