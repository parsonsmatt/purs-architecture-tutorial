module Example.CounterRemPrime where

import Prelude
import Control.Plus (Plus)

import Halogen

import qualified Example.Counter as Counter
import qualified Example.RemGeneric as Rem

type State g = Rem.State Counter.State Counter.Input g
type Query = Rem.Query Counter.Input

ui :: forall g. (Plus g)
   => Component (State g) Query g
ui = Rem.addRemove Counter.ui (Counter.init 0)
