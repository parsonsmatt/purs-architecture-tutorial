module Example.Four where

import Prelude
import Data.Tuple
import Data.Functor.Coproduct
import Data.Array
import Control.Plus (Plus)

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import qualified Example.Counter as CC
import qualified Example.CounterRem as Counter
import Example.Two (CounterSlot(..))
import Example.Three (State(..), addCounter, initialState)

data Input a = AddCounter a

listRemUI :: forall g p. (Functor g)
          => ParentComponentP _ _ _ _ g _ p
listRemUI = component' render eval peek
    where
        render :: Render _ _ _
        render state =
            H.div_ [ H.h1_ [ H.text "Counters" ]
                   , H.ul_ (map (H.slot <<< CounterSlot) state.counterArray)
                   , H.button [ E.onClick $ E.input_ AddCounter ]
                              [ H.text "Add Counter" ]
                   ]

        eval :: EvalP _ _ _ _ _ g _ p
        eval (AddCounter next) = do
            modify addCounter
            pure next

        peek :: Peek _ _ _ _ g _ p
        peek (ChildF counterSlot queryAction) =
            case queryAction of
                 Counter.Remove _ ->
                    modify $ removeCounter counterSlot
                 _ ->
                    pure unit

removeCounter :: CounterSlot -> State -> State
removeCounter (CounterSlot index) state =
    state { counterArray = filter (/= index) state.counterArray }

-- ui :: forall g. (Plus g)
--    => InstalledComponent _ _ _ _ g _ _
-- ui = install' listRemUI mkCounter
--     where
--         mkCounter :: CounterSlot -> Tuple (ComponentP _ _ g _ _) _
--         mkCounter (CounterSlot _) =
--             createChild Counter.ui (installedState unit)
