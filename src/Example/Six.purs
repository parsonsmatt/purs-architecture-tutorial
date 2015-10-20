module Example.Six where

import Prelude
import Control.Plus (Plus)
import Data.Functor.Coproduct (Coproduct(..))
import Control.Monad.Aff (Aff())

import Halogen
import qualified Halogen.HTML.Indexed as H

import qualified Example.Five as Gif

data Input a = NoOp a

type State =
  InstalledState Unit Gif.State Input Gif.Input (Aff (Gif.GifEffects ())) Boolean

type Query =
  Coproduct Input (ChildF Boolean Gif.Input)

ui :: Component State Query (Aff (Gif.GifEffects ()))
ui = parentComponent render eval
  where
    render _ =
      H.div_
        [ H.slot true \_ -> { component: Gif.ui, initialState: Gif.initialState }
        , H.slot false \_ -> { component: Gif.ui, initialState: Gif.initialState }
        ]
    eval :: EvalParent Input Unit Gif.State Input Gif.Input (Aff (Gif.GifEffects ())) Boolean
    eval (NoOp a) = pure a
