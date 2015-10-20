module Example.Four where

import Prelude
import Data.Either
import Data.Functor.Coproduct
import Data.Array (filter)
import Control.Plus (Plus)

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import qualified Example.CounterRem as Counter
import Example.Two (CounterSlot(..))
import Example.Three (State(), mslot, addCounter, initialState)

data Input a = AddCounter a

type StateP g =
  InstalledState State (Counter.StateMiddle g) Input Counter.QueryMiddle g CounterSlot

type QueryP =
  Coproduct Input (ChildF CounterSlot Counter.QueryMiddle)

mapSlot slot comp state index = mslot (slot index) comp state

ui :: forall g. (Plus g)
   => Component (StateP g) QueryP g
ui = parentComponent' render eval peek
  where
    render state =
      H.div_ 
        [ H.h1_ [ H.text "Counters" ]
        , H.ul_ (map (mapSlot CounterSlot Counter.ui (installedState unit)) state.counterArray)
        , H.button [ E.onClick $ E.input_ AddCounter ]
                   [ H.text "Add Counter" ]
        ]

    eval :: EvalParent _ _ _ _ _ g CounterSlot
    eval (AddCounter next) = do
      modify addCounter
      pure next

    peek :: Peek (ChildF CounterSlot Counter.QueryP) State (Counter.StateP g) Input Counter.QueryP g CounterSlot
    peek (ChildF counterSlot (Coproduct queryAction)) =
      case queryAction of
        Left (Counter.Remove _) ->
          modify (removeCounter counterSlot)
        _ ->
          pure unit

removeCounter :: CounterSlot -> State -> State
removeCounter (CounterSlot index) state =
  state { counterArray = filter (/= index) state.counterArray }
