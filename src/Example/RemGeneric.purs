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

addRemove :: forall g s f. (Plus g)
          => Component s f g
          -> s
          -> Component (State s f g) (Query f) g
addRemove comp state = parentComponent render eval
  where
    render _ =
        H.div_ 
          [ H.slot unit \_ -> { component: comp, initialState: state } 
          , H.button [ E.onClick $ E.input_ Remove ]
                     [ H.text "Remove" ]
          ]
    eval :: EvalParent QueryP StateP s QueryP f g ChildSlot
    eval (Remove a) = pure a

type State s f g =
  InstalledState StateP s QueryP f g ChildSlot

type Query f =
  Coproduct QueryP (ChildF ChildSlot f)
