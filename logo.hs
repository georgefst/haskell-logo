{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

{- TODO
turn in to cabal script
    for now:
        cabal install --package-env . --allow-newer='*:base' --lib diagrams-lib diagrams-core diagrams-svg
upload to wiki
    include original, simpler, explicit points version
-}

main :: IO ()
main = mainWith $ d & center & pad 1.1 & lwG 0.5

d :: Diagram B
d =
    position
        [
            ( p2 (0, 0)
            , reflectX (diagonal purple0)
                === reflectX (reflectY (diagonal purple0))
            )
        ,
            ( p2 (120, 0)
            , reflectX (diagonal purple1)
                === ( reflectY (diagonal purple1)
                        <> reflectX (reflectY (diagonal purple1))
                    )
            )
        ,
            ( p2 (145, 75)
            , reflectY $ horizontal purple2 200
            )
        ,
            ( p2 (205, -15)
            , reflectY $ horizontal purple2 140
            )
        ]

diagonal :: Colour Double -> Diagram B
diagonal c =
    translate (V2 (-45) 0) $
        polygonFromCoords
            c
            [ (0, 0)
            , (120, 180)
            , (210, 180)
            , (90, 0)
            ]

horizontal :: Colour Double -> Double -> Diagram B
horizontal c x =
    polygonFromCoords
        c
        [ (0, 0)
        , (40, 60)
        , (x, 60)
        , (x, 0)
        ]

polygonFromCoords :: Colour Double -> [(Double, Double)] -> Diagram B
polygonFromCoords c =
    fc c
        . lc c
        . stroke
        . closeLine
        . fromVertices
        . map p2

-- from haskell.org
purple0 :: Colour Double
purple0 = sRGB24read "#453a62"
purple1 :: Colour Double
purple1 = sRGB24read "#5e5086"
purple2 :: Colour Double
purple2 = sRGB24read "#8f4e8b"
