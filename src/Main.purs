module Main where

import Prelude

import Control.Monad.Aff (runAff)

import Halogen
import Halogen.Util (appendToBody)
import Control.Monad.Eff.Exception (throwException)

import qualified Example.Zero as Ex0
import qualified Example.One as Ex1
import qualified Example.Two as Ex2
import qualified Example.Three as Ex3
import qualified Example.Four as Ex4
import qualified Example.CounterRem as CR
import qualified Example.Five as Ex5
import qualified Example.Six as Ex6
import qualified Example.Seven as Ex7

main = runAff throwException (const (pure unit)) $ do
    -- app <- runEx0
    -- app <- runEx1
    -- app <- runEx2
    -- app <- runEx3
    -- app <- runRemCounter
    -- app <- runEx4
    -- app <- runEx5
    -- app <- runEx6
    app <- runEx7
    appendToBody app.node

runEx0 = runUI Ex0.ui unit
runEx1 = runUI Ex1.ui (Ex1.init 0)
runEx2 = runUI Ex2.ui (installedState Ex2.init)
runEx3 = runUI Ex3.ui (installedState Ex3.initialState)
runRemCounter = runUI CR.ui (installedState unit)
runEx4 = runUI Ex4.ui (installedState Ex3.initialState)
runEx5 = runUI Ex5.ui Ex5.initialState
runEx6 = runUI Ex6.ui (installedState unit)
runEx7 = runUI (Ex7.makeList Ex5.ui Ex5.initialState) Ex7.initialState
