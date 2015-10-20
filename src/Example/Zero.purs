module Example.Zero where

import Prelude

import Halogen
import qualified Halogen.HTML.Indexed as H

data Input a = Input a

ui :: forall g. (Functor g) => Component Unit Input g
ui = component render eval
    where
        render _ =
            H.div_ [ H.h1_ [ H.text "Hello, World!" ] ]
        eval :: Eval Input Unit Input g
        eval (Input a) = pure a
