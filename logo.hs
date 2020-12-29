{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import qualified Data.Text.Lazy.IO as TL
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Graphics.Svg (makeAttribute, prettyText)

{- TODO
turn in to cabal script
    for now:
        cabal install --package-env . --allow-newer='*:base' --lib diagrams-lib diagrams-core diagrams-svg svg-builder
-}

main :: IO ()
main =
    TL.writeFile "out.svg" . prettyText . renderDia SVG opts $
        d & center & scaleY 1.5 & pad 1.1 & lw 0
  where
    opts =
        SVGOptions
            { _size = mkSizeSpec $ V2 (Just 1000) Nothing
            , _svgDefinitions = Nothing
            , _idPrefix = ""
            , _svgAttributes = [makeAttribute "shape-rendering" "crispEdges"]
            , _generateDoctype = True
            }

d :: Diagram B
d =
    position
        [
            ( p2 (70, -40)
            , scale (1 / 6) $ center hs
            )
        ,
            ( p2 (120, 0)
            , reflectX $ diagonal 120 & fc purple0
            )
        ,
            ( p2 (125, 40)
            , reflectX $ horizontal 210 & fc purple0
            )
        ,
            ( 0
            , reflectX $ diagonal 120 & fc purple0
            )
        ,
            ( p2 (-165, -120)
            , horizontal 200 & fc purple1
            )
        ,
            ( 0
            , reflectX $ reflectY $ diagonal 120 & fc purple1
            )
        ,
            ( p2 (160, -40)
            , reflectY $ diagonal 40 & fc purple2
            )
        ,
            ( p2 (115, -40)
            , horizontal 170 & fc purple2
            )
        ,
            ( p2 (35, -120)
            , horizontal 170 & fc purple2
            )
        ]

hs :: Diagram B
hs =
    position
        [
            ( p2 (0, 0)
            , reflectX (diagonal 120) === reflectX (reflectY (diagonal 120))
                & fc grey0
            )
        ,
            ( p2 (120, 0)
            , reflectX (diagonal 120) === (reflectY (diagonal 120) <> reflectX (reflectY (diagonal 120)))
                & fc grey1
            )
        ,
            ( p2 (145, 50)
            , reflectY (horizontalChopped 200)
                & fc grey2
            )
        ,
            ( p2 (205, -10)
            , reflectY (horizontalChopped 140)
                & fc grey2
            )
        ]

diagonal :: Double -> Diagram B
diagonal y =
    translate (V2 (-45) 0) $
        polygonFromCoords
            [ (0, 0)
            , (y, y)
            , (y + 90, y)
            , (90, 0)
            ]

horizontalChopped :: Double -> Diagram B
horizontalChopped x =
    polygonFromCoords
        [ (0, 0)
        , (40, 40)
        , (x, 40)
        , (x, 0)
        ]

horizontal :: Double -> Diagram B
horizontal x =
    polygonFromCoords
        [ (0, 0)
        , (40, 40)
        , (x + 40, 40)
        , (x, 0)
        ]

polygonFromCoords :: [(Double, Double)] -> Diagram B
polygonFromCoords =
    stroke
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
