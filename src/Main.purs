module Main where

import Prelude

import Control.Monad.Aff (runAff)

import Halogen
import Halogen.Util (appendToBody)
import Control.Monad.Eff.Exception (throwException)

import qualified Example.Zero as Ex0
import qualified Example.One as Ex1
import qualified Example.Two as Ex2

--
main = runAff throwException (const (pure unit)) $ do
    -- app <- runEx0
    -- app <- runEx1
    app <- runEx2
    appendToBody app.node

runEx0 = runUI Ex0.ui unit
runEx1 = runUI Ex1.ui (Ex1.init 0)
runEx2 = runUI Ex2.ui (installedState (Ex2.init))
