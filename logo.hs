{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

{- TODO
turn in to cabal script
    for now:
        cabal install --package-env . --allow-newer='*:base' --lib diagrams-lib diagrams-core diagrams-svg
upload to wiki
-}

main :: IO ()
main = mainWith $ explicitPoints & center & pad 1.1

-- | Points taken from <wiki.haskell.org/Thompson-Wheeler_logo wiki>.
explicitPoints :: Diagram B
explicitPoints =
    foldMap
        ( \(colour, points) ->
            let ps = p2 <$> points
             in transform reflectionY . moveTo (NE.head ps) . fc colour . stroke . closeLine . fromVertices $ NE.toList ps
        )
        [
            ( purple0
            , (0, 120)
                :| [ (40, 60)
                   , (0, 0)
                   , (30, 0)
                   , (70, 60)
                   , (30, 120)
                   ]
            )
        ,
            ( purple1
            , (40, 120)
                :| [ (80, 60)
                   , (40, 0)
                   , (70, 0)
                   , (150, 120)
                   , (120, 120)
                   , (95, 82.5)
                   , (70, 120)
                   ]
            )
        ,
            ( purple2
            , (136.666667, 85)
                :| [ (123.333333, 65)
                   , (170, 65)
                   , (170, 85)
                   ]
            )
        ,
            ( purple2
            , (116.666667, 55)
                :| [ (103.333333, 35)
                   , (170, 35)
                   , (170, 55)
                   ]
            )
        ]

-- from haskell.org
purple0 :: Colour Double
purple0 = sRGB24read "#453a62"
purple1 :: Colour Double
purple1 = sRGB24read "#5e5086"
purple2 :: Colour Double
purple2 = sRGB24read "#8f4e8b"
