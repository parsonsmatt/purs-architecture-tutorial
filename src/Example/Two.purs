module Example.Two where

import Prelude

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

type State =
    { topCounter :: CounterSlot
    , bottomCounter :: CounterSlot
    }

init :: State
init = { topCounter: CounterSlot 0, bottomCounter: CounterSlot 1 }

data Input a
    = Reset a

pairUI :: forall g p. (Functor g) 
       => ParentComponent State Ex1.State Input Ex1.Input g CounterSlot p
pairUI = component render eval
    where
        render state =
            H.div_ [ H.slot $ state.topCounter
                   , H.slot $ state.bottomCounter
                   , H.button [ E.onClick $ E.input_ Reset ]
                              [ H.text "Reset!" ]
                   ]
        
        eval :: EvalP Input State Ex1.State Input Ex1.Input g CounterSlot p
        eval (Reset next) = do
            query (CounterSlot 0) (action Ex1.Reset)
            query (CounterSlot 1) (action Ex1.Reset)
            pure next

ui :: forall g p. (Plus g) => InstalledComponent State Ex1.State Input Ex1.Input g CounterSlot p
ui = install pairUI mkCounter
    where
        mkCounter (CounterSlot _) = createChild Ex1.ui (Ex1.init 0)
