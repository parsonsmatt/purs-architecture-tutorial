module Example.Two where

import Prelude

import Data.Functor.Coproduct
import Control.Monad 
import Control.Plus (Plus)

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import qualified Example.Counter as Ex1

newtype CounterSlot = CounterSlot Int

instance counterSlotOrd :: Ord CounterSlot where
  compare (CounterSlot a) (CounterSlot b) = compare a b

instance counterSlotEq :: Eq CounterSlot where
  eq (CounterSlot a) (CounterSlot b) = eq a b

type StateP =
  { topCounter :: CounterSlot
  , bottomCounter :: CounterSlot
  }

init :: StateP
init = { topCounter: CounterSlot 0, bottomCounter: CounterSlot 1 }

data Input a
  = Reset a

type State g =
  InstalledState StateP Ex1.State Input Ex1.Input g CounterSlot
type Query =
  Coproduct Input (ChildF CounterSlot Ex1.Input)

ui :: forall g. (Plus g) 
   => Component (State g) Query g
ui = parentComponent render eval
  where
    render state =
      H.div_
        [ H.slot state.topCounter mkCounter
        , H.slot state.bottomCounter mkCounter 
        , H.button [ E.onClick $ E.input_ Reset ]
                   [ H.text "Reset!" ]
        ]

    mkCounter :: Unit -> { component :: Component Ex1.State Ex1.Input g, initialState :: Ex1.State }
    mkCounter _ = { component: Ex1.ui, initialState: Ex1.init 0 }

    eval :: EvalParent _ _ _ _ _ g _
    eval (Reset next) = do
      query (CounterSlot 0) (action Ex1.Reset)
      query (CounterSlot 1) (action Ex1.Reset)
      pure next
