module Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console

import qualified DOMImport as DOM

import Data.Maybe
import Control.Monad.Eff
import Data.Maybe.Unsafe
import Data.Nullable (toMaybe)
import qualified Example.One as Ex1
import qualified Example.Two as Ex2

main =
    documentBody >>= Ex2.renderCounter

documentBody :: forall eff. Eff (dom :: DOM.DOM | eff) DOM.Element
documentBody = do
    win <- DOM.window
    doc <- DOM.document win
    elm <- fromJust <$> toMaybe <$> DOM.body doc
    return $ DOM.htmlElementToElement elm
