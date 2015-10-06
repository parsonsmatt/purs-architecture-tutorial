module Example.Zero where

import Prelude

import Halogen
import qualified Halogen.HTML.Indexed as H

data Input a = Input a

ui :: forall g p. (Functor g) => Component Unit Input g p
ui = component render eval
    where
        render _ =
            H.div_ [ H.h1 [] [ H.text "Hello, World!" ] ]
        eval :: Eval Input Unit Input g
        eval (Input a) = pure a
