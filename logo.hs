{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text.Encoding (decodeUtf8)
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Graphics.Svg (renderBS)
import Svgone

{- TODO
turn in to cabal script
    for now:
        cabal install --package-env . --allow-newer='*:base' --lib \
            diagrams-lib diagrams-core diagrams-svg svg-builder svgone
less use of absolute positions for 'hs'
-}

main :: IO ()
main = do
    let d = renderDia SVG opts $ hlsWithHs & center & scaleY 1.5 & pad 1.1 & lw 0
    run allPluginsWithDefaults "" (decodeUtf8 . BSL.toStrict $ renderBS d) "out.svg"
  where
    opts =
        SVGOptions
            { _size = mkSizeSpec $ V2 (Just 1000) Nothing
            , _svgDefinitions = Nothing
            , _idPrefix = ""
            , _svgAttributes = []
            , _generateDoctype = True
            }

hlsWithHs :: Diagram B
hlsWithHs =
    center hls
        <> moveTo (p2 (-10, -40)) (scale (1 / 6) (center hs))

hls :: Diagram B
hls =
    alignBL h
        === alignTL
            ( (l & snugB & snugR)
                <> (s & alignBL)
            )

h :: Diagram B
h =
    fc purple0 $
        reflectX $
            ( (diagonal 120 & centerY & snugL) <> (horizontal 210 & center & snugL)
                & snugR
            )
                <> (diagonal 120 & centerY & snugR)

l :: Diagram B
l =
    fc purple1 $
        (horizontal 200 & alignBL)
            <> (diagonal 120 & alignBL)

s :: Diagram B
s =
    fc purple2 $
        (horizontal 170 & alignTR)
            <> ( (diagonal 40 & reflectY & alignTL)
                    <> (horizontal 170 & alignBL)
                    & alignBR
                    & snugR
               )

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
            , reflectX (diagonal 120)
                === ( (diagonal 120 & reflectY & reflectX)
                        <> (diagonal 120 & reflectY & snugR)
                    )
                & fc grey1
            )
        ,
            ( p2 (100, 50)
            , reflectY (horizontalChopped 200)
                & fc grey2
            )
        ,
            ( p2 (160, -10)
            , reflectY (horizontalChopped 140)
                & fc grey2
            )
        ]

diagonal :: Double -> Diagram B
diagonal y =
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
