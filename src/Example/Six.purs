module Example.Six where

import Prelude
import Control.Plus (Plus)
import Control.Monad.Aff (Aff())

import Halogen
import qualified Halogen.HTML.Indexed as H

import qualified Example.Five as Gif

data Input a = NoOp a

pairUI :: forall p. ParentComponent Unit Gif.State Input Gif.Input (Aff (Gif.GifEffects ())) Boolean p
pairUI = component render eval
  where
    render _ =
      H.div_
        [ H.slot true
        , H.slot false
        ]
    eval :: EvalP Input Unit Gif.State Input Gif.Input (Aff (Gif.GifEffects ())) Boolean p
    eval (NoOp a) = pure a

ui :: forall p. InstalledComponent Unit Gif.State Input Gif.Input (Aff (Gif.GifEffects ())) Boolean p
ui = install pairUI mkGif
  where
    mkGif _ = createChild Gif.ui (Gif.initialState)
