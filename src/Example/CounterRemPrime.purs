module Example.CounterRemPrime where

import Prelude
import Control.Plus (Plus)

import Halogen

import qualified Example.Counter as Counter
import qualified Example.RemGeneric as Rem

type State g p = Rem.State Counter.State Counter.Input g p
type Query = Rem.Query Counter.Input

ui :: forall g p. (Plus g)
   => Component (Rem.State Counter.State Counter.Input g p)
                (Rem.Query Counter.Input) g p
ui = Rem.addRemove Counter.ui (Counter.init 0)
