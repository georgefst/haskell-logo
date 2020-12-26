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
-}

main :: IO ()
main = mainWith $ d & center & pad 1.1 & lwG 0.5

d :: Diagram B
d =
    position
        [
            ( p2 (125, 60)
            , reflectX $ horizontal purple0
            )
        ,
            ( p2 (195, 75)
            , reflectY $ horizontalChopped purple2 150
            )
        ,
            ( p2 (255, -15)
            , polygonFromCoords
                purple2
                [ (0, 0)
                , (-20, 30)
                , (70, 30)
                , (90, 0)
                ]
            )
        ,
            ( p2 (205, -15)
            , reflectY $ horizontalChopped purple2 140
            )
        ,
            ( p2 (-165, -180)
            , horizontal purple1
            )
        ,
            ( p2 (0, 0)
            , reflectX (diagonal purple0)
                === reflectX (reflectY (diagonal purple1))
            )
        ,
            ( p2 (120, 0)
            , reflectX (diagonal purple0)
                === ( reflectY (diagonal grey2)
                        <> reflectX (reflectY (diagonal grey2))
                    )
            )
        ,
            ( p2 (145, 75)
            , reflectY $ horizontalChopped grey2 200
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

horizontalChopped :: Colour Double -> Double -> Diagram B
horizontalChopped c x =
    polygonFromCoords
        c
        [ (0, 0)
        , (40, 60)
        , (x, 60)
        , (x, 0)
        ]

horizontal :: Colour Double -> Diagram B
horizontal c =
    polygonFromCoords
        c
        [ (0, 0)
        , (40, 60)
        , (250, 60)
        , (210, 0)
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
grey0 :: Colour Double
grey0 = sRGB24read "#666666"
grey1 :: Colour Double
grey1 = sRGB24read "#999999"
grey2 :: Colour Double
grey2 = sRGB24read "#bbbbbb"
