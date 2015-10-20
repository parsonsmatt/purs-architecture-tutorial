module Example.CounterRem where

import Prelude
import Control.Plus (Plus)
import Data.Functor.Coproduct

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import Example.Three (mslot)
import Example.Two (CounterSlot(..))
import qualified Example.Counter as Counter

data Input a = Remove a

type State g =
  InstalledState Unit Counter.State Input Counter.Input g CounterSlot
type Query =
  Coproduct Input (ChildF CounterSlot Counter.Input)

ui :: forall g. (Plus g)
   => Component (State g) Query g
ui = parentComponent render eval
  where
    render _ =
        H.div_ 
          [ mslot (CounterSlot 0) Counter.ui (Counter.init 0)
          , H.button [ E.onClick $ E.input_ Remove ]
                     [ H.text "Remove" ]
          ]
    eval :: EvalParent Input Unit Counter.State Input Counter.Input g CounterSlot
    eval (Remove a) = pure a
