module Main (main) where

import Graphics.Gloss

main :: IO ()
main = display window background picture
  where
    window = InWindow "Hello, Gloss!" (800, 400) (100, 100)
    background = white
    picture = scale 0.5 0.5 $ color black $ text "Hello, World!"
