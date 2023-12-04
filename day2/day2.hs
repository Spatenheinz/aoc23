#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

import Text.Parsec

data Color = Red Int | Green Int | Blue Int

data Game = Game { red :: Int, green :: Int, blue :: Int}
  deriving (Show)

instance Semigroup Game where
  Game r1 g1 b1 <> Game r2 g2 b2 = Game (r1 `max` r2) (g1 `max` g2) (b1 `max` b2)

instance Monoid Game where
  mempty = zero

setting = Game 12 13 14
zero = Game 0 0 0

validGame :: Game -> Bool
validGame (Game r g b) = r <= 12 && g <= 13 && b <= 14

power :: Game -> Int
power (Game r g b) = r * g * b

type Parser a = Parsec String () a

parseColor :: Parser Color
parseColor = do
  x <- read <$> many1 digit
  spaces
  choice [ Red x <$ string "red", Green x <$ string "green", Blue x <$ string "blue"]

parseSubGame :: Parser Game
parseSubGame = do
  colors <- sepBy parseColor (char ',' >> spaces)
  return $ foldr (\x (Game r g b) ->
                    case x of
                      Red x -> Game (r + x) g b
                      Green x -> Game r (g + x) b
                      Blue x -> Game r g (b + x)) zero colors

parseGame :: Parser (Int, [Game])
parseGame = do
  string "Game" >> spaces
  nr <- read <$> many1 digit
  string ":" >> spaces
  game <- sepBy parseSubGame (char ';' >> spaces) <* newline
  return (nr, game)

parseFile :: String -> [(Int, [Game])]
parseFile s = case parse (many parseGame <* eof) "" s of
  Left err -> error $ show err
  Right games -> games

task1 = do
  input <- readFile "input.txt"
  print . sum . map fst . filter (all validGame . snd) $ parseFile input

task2 = do
  input <- readFile "input.txt"
  print . sum . map (power . mconcat . snd) $ parseFile input

main = do
  task1
  task2
