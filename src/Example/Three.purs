module Example.Three where

import Prelude
import Data.Array
import Data.Functor.Coproduct (Coproduct(..))
import Control.Plus (Plus)

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import qualified Example.Counter as Counter
import Example.Two (CounterSlot(..))

type StateP =
  { counterArray :: Array Int
  , nextID :: Int
  }

initialState :: StateP
initialState =
  { counterArray: []
  , nextID: 0
  }

data Input a
  = AddCounter a
  | RemoveCounter a

type State g =
  InstalledState StateP Counter.State Input Counter.Input g CounterSlot

type Query =
  Coproduct Input (ChildF CounterSlot Counter.Input)

mslot :: forall s f g p i. p -> Component s f g -> s -> HTML (SlotConstructor s f g p) i
mslot slot comp state = H.slot slot \_ -> { component: comp, initialState: state }

ui :: forall g. (Plus g)
   => Component (State g) Query g
ui = parentComponent render eval
  where
    render state = 
      H.div_ 
        [ H.h1_ [ H.text "Counters" ]
        , H.ul_ $ map mkSlot state.counterArray
        , H.button [ E.onClick $ E.input_ AddCounter ]
                   [ H.text "Add Counter" ]
        , H.button [ E.onClick $ E.input_ RemoveCounter ]
                   [ H.text "Remove Counter" ]
        ]

    mkSlot i = mslot (CounterSlot i) Counter.ui (Counter.init 0)

    eval :: EvalParent Input StateP Counter.State Input Counter.Input g CounterSlot
    eval (AddCounter next) = do
      modify addCounter
      pure next
    eval (RemoveCounter next) = do
      modify removeCounter
      pure next

addCounter :: StateP -> StateP
addCounter s =
  s { counterArray = s.nextID : s.counterArray
    , nextID = s.nextID + 1
    }

removeCounter :: StateP -> StateP
removeCounter s =
  s { counterArray = drop 1 s.counterArray
    , nextID = s.nextID - 1
    }
