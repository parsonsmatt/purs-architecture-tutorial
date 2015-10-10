module Example.RemGeneric where

import Prelude
import Control.Plus (Plus)
import Data.Functor.Coproduct (Coproduct(..))

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

data QueryP a = Remove a

type ChildSlot = Unit

type StateP = Unit

withRemove :: forall g p s' f'. (Functor g)
           => ParentComponent StateP s' QueryP f' g ChildSlot p
withRemove = component render eval
    where
        render :: Render StateP QueryP ChildSlot
        render _ =
            H.div_ [ H.slot unit
                   , H.button [ E.onClick $ E.input_ Remove ]
                              [ H.text "Remove" ]
                   ]
        eval :: EvalP QueryP StateP s' QueryP f' g ChildSlot p
        eval (Remove a) = pure a

type State s f g p =
    InstalledState StateP s QueryP f g ChildSlot p

type Query f =
    Coproduct QueryP (ChildF ChildSlot f)

addRemove :: forall s f g p. (Plus g)
          => Component s f g p 
          -> s 
          -> Component (State s f g p) (Query f) g p
addRemove comp state = install withRemove mkChild
    where
        mkChild _ = createChild comp state
