module Example.Three where

import Prelude
import Data.Array
import Control.Plus (Plus)

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import qualified Example.Counter as Counter
import Example.Two (CounterSlot(..))

type State =
  { counterArray :: Array Int
  , nextID :: Int
  }

initialState :: State
initialState =
  { counterArray: []
  , nextID: 0
  }

data Input a
  = AddCounter a
  | RemoveCounter a

listUI :: forall g p. (Functor g)
       => ParentComponent State Counter.State Input Counter.Input g CounterSlot p
listUI = component render eval
  where
    render :: Render State Input CounterSlot
    render state = 
      H.div_ 
        [ H.h1_ [ H.text "Counters" ]
        , H.ul_ $ map (H.slot <<< CounterSlot) state.counterArray
        , H.button [ E.onClick $ E.input_ AddCounter ]
                   [ H.text "Add Counter" ]
        , H.button [ E.onClick $ E.input_ RemoveCounter ]
                   [ H.text "Remove Counter" ]
        ]
      eval :: EvalP Input State Counter.State Input Counter.Input g CounterSlot p
      eval (AddCounter next) = do
        modify addCounter
        pure next
      eval (RemoveCounter next) = do
        modify removeCounter
        pure next

addCounter :: State -> State
addCounter s =
  s { counterArray = s.nextID : s.counterArray
    , nextID = s.nextID + 1
    }

removeCounter :: State -> State
removeCounter s =
  s { counterArray = drop 1 s.counterArray
    , nextID = s.nextID - 1
    }

ui :: forall g p. (Plus g) => InstalledComponent State Counter.State Input Counter.Input g CounterSlot p
ui = install listUI mkCounter
  where
    mkCounter (CounterSlot _) = createChild Counter.ui (Counter.init 0)
