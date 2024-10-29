{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
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
import Data.Foldable
import Data.Text.Encoding (decodeUtf8)
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (arrow)
import Graphics.Svg (renderBS)
import Svgone qualified

main :: IO ()
main = do
    for_
        [ (haskell, "haskell", Just "haskell-raw")
        , (survey, "survey", Nothing)
        , (hls, "hls", Nothing)
        ]
        \(d, name, nameRaw) -> do
            let d' = renderBS . renderDia SVG opts $ d & scaleY 1.5 & center & pad 1.1 & lw 0
            maybe mempty (flip BSL.writeFile d' . (<> ".svg")) nameRaw
            Svgone.run Svgone.allPluginsWithDefaults "" (decodeUtf8 $ BSL.toStrict d') $ name <> ".svg"
  where
    opts =
        SVGOptions
            { _size = mkSizeSpec $ V2 (Just 1000) Nothing
            , _svgDefinitions = Nothing
            , _idPrefix = ""
            , _svgAttributes = []
            , _generateDoctype = True
            }

haskell :: Diagram B
haskell =
    hcat'
        (def & catMethod .~ Distrib & sep .~ 80)
        [ arrow & fc purple0
        , lambda & fc purple1
        , equals & fc purple2
        ]

hsGrey :: Diagram B
hsGrey =
    hcat'
        (def & catMethod .~ Distrib & sep .~ 80)
        [ arrow & fc grey0
        , lambda & fc grey1
        , equals & fc grey2
        ]

hls :: Diagram B
hls =
    center hlsWithoutHs
        <> moveTo (p2 (-10, -40)) (scale (1 / 6) (center hsGrey))

hlsWithoutHs :: Diagram B
hlsWithoutHs =
    alignBL (letterH & fc purple0)
        === alignTL
            ( (letterL & fc purple1 & snugB & snugR)
                <> (letterS & fc purple2 & alignBL)
            )

letterH :: Diagram B
letterH =
    reflectX $
        ( (diagonal 120 & centerY & snugL)
            <> (horizontal 210 & center & snugL)
                & snugR
        )
            <> (diagonal 120 & centerY & snugR)

letterL :: Diagram B
letterL =
    (horizontal 200 & alignBL)
        <> (diagonal 120 & alignBL)

letterS :: Diagram B
letterS =
    (horizontal 170 & alignTR)
        <> ( (diagonal 40 & reflectY & alignTL)
                <> (horizontal 170 & alignBL)
                    & alignBR
                    & snugR
           )

survey :: Diagram B
survey =
    hcat'
        (def & catMethod .~ Distrib & sep .~ 80)
        [ arrow & fc purple0
        , lambda & fc purple1
        , checkbox
        ]

arrow :: Diagram B
arrow =
    reflectX (diagonal 120)
        === reflectX (reflectY (diagonal 120))
        & snugR

lambda :: Diagram B
lambda =
    reflectX (diagonal 120)
        === ( (diagonal 120 & reflectY & reflectX)
                <> (diagonal 120 & reflectY)
            )

equals :: Diagram B
equals =
    vsep
        20
        [ reflectY (horizontalChopped 200)
        , reflectY (horizontalChopped 140)
        ]
        & centerY
        & translateX 150 -- TODO something more principled (`snugL` should work but envelope isn't tight enough)

checkbox :: Diagram B
checkbox =
    let
        boxHeight = 100 -- matches the combined height of the lines in the original logo
        tickWidth = 55 -- consistent with current diagonal line widths of 35 and 90
        (tickHeightL, tickHeightR) = (75, 175) & both %~ (* (tickWidth / 90)) -- arbitrary, looks good
        gapX = tickWidth -- doesn't have to be equal, but it looks about right
     in
        mconcat
            [ composeAligned
                snugB
                mconcat
                [ diagonal' tickWidth tickHeightL & reflectX & centerY
                , diagonal' tickWidth tickHeightR
                ]
                & snugL
                & translateX gapX
                & fc purple0
            , reflectY (horizontalChopped' 100 (tickWidth + gapX * 2 + (boxHeight + tickHeightL) / 2))
                & fc purple2
                & snugL
            ]

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

horizontal :: Double -> Diagram B
horizontal x =
    polygonFromCoords
        [ (0, 0)
        , (40, 40)
        , (x + 40, 40)
        , (x, 0)
        ]
        & centerY
        & snugR

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
grey0 :: Colour Double
grey0 = sRGB24read "#666666"
grey1 :: Colour Double
grey1 = sRGB24read "#999999"
grey2 :: Colour Double
grey2 = sRGB24read "#bbbbbb"
