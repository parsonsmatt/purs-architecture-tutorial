module Example.One where

import Prelude

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

type State =
  { count :: Int
  }

data Input a
  = Increment a
  | Decrement a

init :: Int -> State
init i = { count: i }

ui :: forall g. (Functor g) => Component State Input g
ui = component render eval
  where
    render :: Render State Input
    render state =
      H.div_ 
        [ H.button [ E.onClick $ E.input_ Decrement ] 
                   [ H.text "-" ]
        , H.p_ [ H.text (show state.count)]
        , H.button [ E.onClick $ E.input_ Increment ] 
                   [ H.text "+" ]
        ]

    eval :: Eval Input State Input g
    eval (Increment next) = do
      modify (\state -> state { count = state.count + 1 }) 
      pure next
    eval (Decrement next) = do
      modify (\state -> state { count = state.count - 1 })
      pure next
