module Example.Counter where

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
  | Reset a

init :: Int -> State
init i = { count: i }

ui :: forall g p. (Functor g) => Component State Input g p
ui = component render eval
  where
    render :: Render State Input p
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
    eval (Reset next) = do
        modify (const (init 0))
        pure next
