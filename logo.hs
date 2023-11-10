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
allow-newer:
    reanimate-svg:mtl,
    reanimate-svg:transformers,
package reanimate-svg
    ghc-options: -fsimpl-tick-factor=1000
-}

module Main (main) where

import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as T
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Graphics.Svg (renderBS)
import Svgone qualified

main :: IO ()
main = do
    let d = renderBS . renderDia SVG opts $ diag & center & pad 1.1 & lw 0
    T.writeFile "out-raw.svg" $
        -- TODO this is a hack, obviously
        T.intercalate "<defs><style type='text/css'>@font-face {font-family: johnston;src: url('new-johnston-medium-4.ttf');}</style></defs>" . T.splitOn "<defs></defs>" $
            decodeUtf8 (BSL.toStrict d)
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

diag :: Diagram B
diag =
    hcat'
        (def & catMethod .~ Distrib & sep .~ 80)
        [ reflectX (diagonal 120)
            === reflectX (reflectY (diagonal 120))
            & fc purple0
            & skew
            & snugR
        , reflectX (diagonal 120)
            === ( (diagonal 120 & reflectY & reflectX)
                    <> (diagonal 120 & reflectY)
                )
            & fc purple1
            & skew
        , vcat'
            (def & catMethod .~ Distrib & sep .~ 90)
            [ mconcat
                [ text "LONDON HASKELL"
                    & font "johnston"
                    & fc white
                    -- these remaining values are based on imprecise aesthetic judgements
                    & translateX -4.5
                    & scale 41
                    & translateY -3 --
                    -- above is optimised for Firefox, but this is needed in addition for eog/rsvg
                    -- although they struggle with loading the custom font in the first place
                    -- & translateY -12
                , reflectY (horizontalChopped (200 + tubeExtension + tubeLeftPad))
                    & fc (sRGB24read "#000f9f")
                    & skew
                , annularWedge 150 97 xDir (1 @@ turn)
                    & fc (sRGB24read "#e1251b")
                    & translateX -tubeExtension
                ]
            , reflectY (horizontalChopped (140 + tubeExtension + tubeLeftPad))
                & fc purple2
                & skew
            ]
            & translateY 45
            & translateX (140 + tubeExtension + tubeLeftPad)
        ]
  where
    tubeExtension = 170 -- necessary - ensures roundel fits while having correct overhang on right
    tubeLeftPad = 25 -- arbitrary - purely a matter of aesthetic judgement
    skew = scaleY 1.5

diagonal :: Double -> Diagram B
diagonal y =
    polygonFromCoords
        [ (0, 0)
        , (y, y)
        , (y + 90, y)
        , (90, 0)
        ]
        & translateX -45

horizontalChopped :: Double -> Diagram B
horizontalChopped x =
    polygonFromCoords
        [ (0, 0)
        , (40, 40)
        , (x, 40)
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
