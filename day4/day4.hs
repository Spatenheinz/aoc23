#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

import Text.Parsec
import Data.Char (isDigit)
import Data.Maybe
