#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

import Text.Parsec
import Data.Maybe
import Debug.Trace
import Data.List

type Parser a = Parsec String () a

lexeme :: Parser a -> Parser a
lexeme = (<* spaces)

lexemeInt :: Parser a -> Parser a
lexemeInt = (<* skipMany (oneOf " \t"))

intP :: Parser Int
intP = lexemeInt $ read <$> many1 digit

intsP :: Parser [Int]
intsP = many1 intP

seedP :: Parser [Int]
seedP =  lexeme (string "seeds:") >> intsP <* newline <* newline

mapsP :: Parser [(Int, Int, Int)]
mapsP = do
  startname
  lexeme (string "map:")
  sepEndBy1 (toRanges <$> count 3 intP) (many newline)
  where
    startname =
      choice $ map (lexeme . try . string)
             [ "seed-to-soil", "soil-to-fertilizer", "fertilizer-to-water", "water-to-light"
             , "light-to-temperature", "temperature-to-humidity", "humidity-to-location"
             ]

toRanges :: [Int] -> (Int, Int, Int)
toRanges [dst, src, r] = (dst, src, r)

inrange :: Int -> (Int, Int, Int) -> Bool
inrange x (dst, src, r) = x >= src && x <= src + r - 1

offset :: Int -> Int -> Int -> Int
offset x fStart toStart = x - (fStart - toStart)



parseFile :: String -> ([Int], [[(Int, Int, Int)]])
parseFile s = case parse (do { x <- seedP; y <- many mapsP; return (x,y)}) "" s of
  Left e -> error (show e)
  Right r -> r

foldf :: [(Int,Int, Int)] -> [Int] -> [Int]
foldf maps = map (\x -> case filter (inrange x) maps of
                          [] -> x
                          ((dst,src,_):_) -> offset x src dst)

loop :: [(Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
loop [] _ acc = acc
loop ((s,e):xs) maps acc =
  let (next, this, matched) =
        foldr (\ (md, ms, mr) (nextGen, thisGen, matched) ->
           let os = max s ms
               oe = min e (ms + mr - 1)
            in if os < oe then
               let res = (os - ms + md, oe - ms + md) : nextGen
                   os' = if os > s then (s, os-1) : thisGen else thisGen
                   oe' = if oe < e then (oe+1, e) : os' else os'
                in (res, oe', True)
             else
                (nextGen, thisGen, matched)
        ) (acc, xs, False) maps
  in loop this maps (if matched then next else (s,e):next)


task1 (seeds, mapss) = do
  print $ minimum $ foldl (flip foldf) seeds mapss

pairwise :: [a] -> [(a,a)]
pairwise [] = []
pairwise (x:y:xs) = (x,y):pairwise xs


task2 (seeds, mapss) = do
  let m = map (\(x,y) -> (x, x+y-1)) $ pairwise seeds
  print $ fst . minimum $ foldl (\acc e -> loop acc e []) m mapss
--   print $ foldl' (\acc x -> minimum $ acc : foldl (flip foldf) x mapss) (maximum seeds) m

main = do
  s <- readFile "input.txt"
  task1 $ parseFile s
  task2 $ parseFile s
