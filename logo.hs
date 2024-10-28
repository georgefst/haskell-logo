{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

{- cabal:
build-depends:
    base >= 4.16,
    bytestring,
    diagrams-core,
    diagrams-lib,
    diagrams-svg,
    svg-builder,
    svgone,
    text,
-}
{- project:
-- TODO my package - will be updated imminently
allow-newer: svgone:*,
-- TODO https://github.com/reanimate/reanimate-svg/pull/45
allow-newer:
    reanimate-svg:mtl,
    reanimate-svg:transformers,
package reanimate-svg
    ghc-options: -fsimpl-tick-factor=300
-}

module Main (main) where

import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Text.Encoding (decodeUtf8)
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Graphics.Svg (renderBS)
import Svgone qualified

main :: IO ()
main = do
    let d = renderBS . renderDia SVG opts $ hs & center & pad 1.1 & lw 0
    BSL.writeFile "out-raw.svg" d
    Svgone.run Svgone.allPluginsWithDefaults "" (decodeUtf8 $ BSL.toStrict d) "out.svg"
  where
    opts =
        SVGOptions
            { _size = mkSizeSpec $ V2 (Just 1000) Nothing
            , _svgDefinitions = Nothing
            , _idPrefix = ""
            , _svgAttributes = []
            , _generateDoctype = True
            }

hs :: Diagram B
hs =
    hcat'
        (def & catMethod .~ Distrib & sep .~ 80)
        [ reflectX (diagonal 120)
            === reflectX (reflectY (diagonal 120))
            & fc purple0
            & snugR
        , reflectX (diagonal 120)
            === ( (diagonal 120 & reflectY & reflectX)
                    <> (diagonal 120 & reflectY)
                )
            & fc purple1
        , vsep
            20
            [ reflectY (horizontalChopped 200)
                & fc purple2
            , reflectY (horizontalChopped 140)
                & fc purple2
            ]
            & centerY
            & translateX 150 -- TODO something more principled (`snugL` should work but envelope isn't tight enough)
        ]
        & scaleY 1.5

diagonal :: Double -> Diagram B
diagonal = diagonal' 90
diagonal' :: Double -> Double -> Diagram B
diagonal' h y =
    polygonFromCoords
        [ (0, 0)
        , (y, y)
        , (y + h, y)
        , (h, 0)
        ]
        & translateX -(h / 2)

horizontalChopped :: Double -> Diagram B
horizontalChopped = horizontalChopped' 40
horizontalChopped' :: Double -> Double -> Diagram B
horizontalChopped' h x =
    polygonFromCoords
        [ (0, 0)
        , (h, h)
        , (x, h)
        , (x, 0)
        ]
        & centerY
        & snugR

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
