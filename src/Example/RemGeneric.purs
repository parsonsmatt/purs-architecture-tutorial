module Example.RemGeneric where

import Prelude
import Control.Plus (Plus)
import Data.Functor.Coproduct (Coproduct(..))

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

data QueryP a = Remove a

type State s f g =
  InstalledState Unit s QueryP f g Unit

type Query f =
  Coproduct QueryP (ChildF Unit f)

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
    eval :: EvalParent QueryP Unit s QueryP f g Unit
    eval (Remove a) = pure a
