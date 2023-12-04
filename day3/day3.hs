#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

import Text.Parsec
import Data.Char (isDigit)
import Data.Maybe

type Parser a = Parsec String () a

type Span = (Int, Int)
type Pos = (Int, Int)

data N = N { _num :: Int, span :: Span, nline :: Int }
  deriving (Show)
data S = S { _char :: Char, index :: Int, sline :: Int }
  deriving (Show)

data Subject = Number N | Symbol S

data Scheme = Scheme { nums :: [N] , syms :: [S] }
  deriving (Show)

instance Semigroup Scheme where
  Scheme n1 s1 <> Scheme n2 s2 = Scheme (n1 <> n2) (s1 <> s2)

instance Monoid Scheme where
  mempty = Scheme mempty mempty

parseSubject :: Parser Subject
parseSubject = do
  pos <- getPosition
  let line = sourceLine pos
  let index = sourceColumn pos
  choice [Number <$> parseNum index line, Symbol <$> parseSym index line]

parseNum :: Int -> Int -> Parser N
parseNum index line = do
  x <- many1 digit
  maxx <- sourceColumn <$> getPosition
  return $ N (read x) (index, maxx - 1) line

parseSym :: Int -> Int -> Parser S
parseSym index line = do
  x <- satisfy (\x -> (not . isDigit) x && x `notElem` ".\n")
  return $ S x index line

parseLine :: Parser [Subject]
parseLine = do
  many (char '.')
  subjects <- many (parseSubject <* many (char '.'))
  newline
  return subjects

parseFile :: String -> Scheme
parseFile s = case parse (many parseLine <* eof) "" s of
  Left err -> error $ show err
  Right games -> mkScheme . concat $ games

mkScheme :: [Subject] -> Scheme
mkScheme = foldr (\x (Scheme ns ss) -> case x of
                                          Number n -> Scheme (n:ns) ss
                                          Symbol s -> Scheme ns (s:ss)) mempty

isAdj :: S -> N -> Bool
isAdj (S c1 x1 y1) (N _ (xmin, xmax) y2) =
  abs (y2 - y1) <= 1 && xmin - 1 <= x1 && x1 <= xmax + 1

checkSyms, checkSyms2 :: Scheme -> Int
checkSyms scheme =
  sum .
  map _num .
  filter (\num -> any (`isAdj` num) (syms scheme)) $ nums scheme

checkSyms2 scheme = sum .
  mapMaybe (\s -> case (_char s, filter (isAdj s) $ nums scheme) of
                    ('*', [n1, n2]) -> Just $ _num n1 * _num n2
                    _ -> Nothing) $ syms scheme

task1 = print . checkSyms

task2 = print . checkSyms2

main = do
  input <- readFile "input.txt"
  let scheme = parseFile input
  task1 scheme
  task2 scheme
