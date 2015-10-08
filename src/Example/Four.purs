module Example.Four where

import Prelude
import Data.Array
import Control.Plus (Plus)

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import qualified Example.CounterRem as Counter
import Example.Two (CounterSlot(..))
import Example.Three

listRemUI :: forall g p. (Functor g)
          => ParentComponentP State Counter.State Input Counter.Input g CounterSlot p
listRemUI = component' render eval peek
    where
        render :: Render State Input CounterSlot
        render state =

        eval :: EvalP Input State Counter.State Input Counter.Input g CounterSlot p
        eval (AddCounter next) = do
            modify addCounter
            pure next
        eval (RemoveCounter next) = do
            modify removeCounter
            pure next

        peek = 
