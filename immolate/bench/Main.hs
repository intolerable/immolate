module Main where

import Control.Monad
import Criterion.Main
import Data.Text.Lazy (Text)
import qualified Lucid

import Immolate

main :: IO ()
main = defaultMain
  [ bgroup "lots of text" 
    [ bench "lucid" $
        whnf lucidLotsOfText "this is an example string"
    , bench "immolate" $
        whnf immolateLotsOfText "this is an example string"
    ]
  , bgroup "lots of h1 elements" 
    [ bench "lucid" $
        whnf lucidLotsOfH1s "this is an example string"
    , bench "immolate" $
        whnf immolateLotsOfH1s "this is an example string"
    ]
  , bgroup "astonishingly-nested divs" 
    [ bench "lucid" $ 
        whnf (lucidNestedDivs mempty) 100
    , bench "immolate" $
        whnf (immolateNestedDivs mempty) 100
    ]
  ]

lucidLotsOfText :: Text -> Text
lucidLotsOfText t = Lucid.renderText $ 
  replicateM 100 (Lucid.toHtml t)

immolateLotsOfText :: Text -> Text
immolateLotsOfText t = renderText $ 
  replicateM 100 $ toHtml t

lucidLotsOfH1s :: Text -> Text
lucidLotsOfH1s t = Lucid.renderText $ 
  replicateM 100 $ Lucid.h1_ (Lucid.toHtml t)

immolateLotsOfH1s :: Text -> Text 
immolateLotsOfH1s t = renderText $
  replicateM 100 $ h1_ $ toHtml t

lucidNestedDivs :: Lucid.Html () -> Int -> Text
lucidNestedDivs acc 0 = Lucid.renderText acc
lucidNestedDivs acc n = lucidNestedDivs (Lucid.div_ acc) (pred n)

immolateNestedDivs :: Html () -> Int -> Text
immolateNestedDivs acc 0 = renderText acc
immolateNestedDivs acc n = immolateNestedDivs (div_ acc) (pred n)

