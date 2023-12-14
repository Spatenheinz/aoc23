#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [])"

import Data.List

reflecBoth nDiffs xs =
  let row = diffList nDiffs xs
      col = diffList nDiffs $ transpose xs
  in map (*100) row ++ col

task1 inp = do
  print $ reflecBoth 0 inp

main = do
  inp <- lines <$> readFile "input.txt"

  task1 inp

(...) = (.).(.)

diffList nDiffs xs =
  [ i | (i,l,r) <- zip3 [0..] (inits xs) (tails xs)
  , not (null l), not (null r)
  , nDiffs == sum (zipWith (sum ... zipWith (\x y -> if y == x then 0 else 1))
                   (reverse l) r)
  ]
