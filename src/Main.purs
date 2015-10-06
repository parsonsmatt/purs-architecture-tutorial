module Main where

import Prelude

import Control.Monad.Aff (runAff)

import Halogen
import Halogen.Util (appendToBody)
import Control.Monad.Eff.Exception (throwException)

import qualified Example.Zero as Ex0

main = runAff throwException (const (pure unit)) $ do
    node <- runUI Ex0.ui unit
    appendToBody node.node
