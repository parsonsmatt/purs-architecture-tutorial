module Example.CounterRem where

import Prelude
import Control.Plus (Plus)

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import Example.Two (CounterSlot(..))
import qualified Example.Counter as Counter

type State = Unit

data Input a
    = Remove a

init :: State
init = unit

withRemove :: forall g p. (Functor g)
           => ParentComponent State Counter.State Input Counter.Input g CounterSlot p
withRemove = component render eval
    where
        render _ =
            H.div_ [ H.slot (CounterSlot 0)
                   , H.button [ E.onClick $ E.input_ Remove ]
                              [ H.text "Remove" ]
                   ]
        eval (Remove a) = pure a

ui :: forall g p. (Plus g)
   => InstalledComponent State Counter.State Input Counter.Input g CounterSlot p
ui = install withRemove mkCounter
    where
        mkCounter (CounterSlot _) = createChild Counter.ui (Counter.init 0)
