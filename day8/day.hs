#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [])"

import qualified Data.Map.Strict as M
import Text.Parsec
import Data.List (isSuffixOf)
import Data.Bifunctor (first)

data D = L | R deriving (Show, Eq)
type Parser = Parsec String ()

parseLines = many1 $ do
  from <- many1 letter <* spaces <* char '=' <* spaces <* char '('
  to <- many1 letter <* char ',' <* spaces
  to2 <- many1 letter <* string ")" <* spaces
  return (from, (to, to2))
parseSeq = many1 $ do { d <- oneOf "LR"; return $ if d == 'L' then L else R }
parseFull = do { x <- parseSeq <* spaces;
                y <- parseLines <* eof; return (x, M.fromList y) }
parse_ s = case parse parseFull "" s of
  Left err -> error $ show err
  Right paths -> paths

find :: [D] -> M.Map String (String, String) -> String -> Int -> Int
find _ _ "ZZZ" acc = acc
find (d:ds) map_ from acc =
  let (to, to2) = map_ M.! from
      to' = if d == L then to else to2
  in find ds map_ to' (acc + 1)

findMany :: [D] -> M.Map String (String, String) -> [String] -> Int
findMany ds map_ froms = foldr1 lcm cycles
  where cycles = froms >>= \from -> [find ds map_ from 0]
        find (d:ds) map_ from acc =
          let (to, to2) = map_ M.! from
              to' = if d == L then to else to2
          in if "Z" `isSuffixOf` to' then acc + 1 else find ds map_ to' (acc + 1)

task1 (seq, map_) = do
  print $ find seq map_ "AAA" 0

task2 (seq, map_) = do
  let starts = filter (isSuffixOf "A") $ M.keys map_
  print $ findMany seq map_ starts

main = do
  input <- readFile "input.txt"
  let parsed = first cycle $ parse_ input
  task1 parsed
  task2 parsed
