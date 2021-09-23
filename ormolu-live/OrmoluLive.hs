module Main where

import Ormolu

main :: IO ()
main = print =<< ormolu defaultConfig "<input>" "data  A = T \n  |     X a"
