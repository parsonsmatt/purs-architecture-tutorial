module Example.Two where

import Prelude ((<<<))
import Control.Monad.Eff

import qualified Example.One as One

import qualified Thermite as T
import qualified Thermite.Action as T

import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP

type Model =
    { topCounter :: One.Model
    , bottomCounter :: One.Model
    }

init :: Int -> Int -> Model
init t b =
    { topCounter: t
    , bottomCounter: b
    }

data Action
    = Reset
    | Top One.Action
    | Bottom One.Action

update :: Action -> Model -> Model
update Reset _ =
    init 0 0
update (Top action) model =
    model { topCounter = One.update action model.topCounter }
update (Bottom action) model =
    model { bottomCounter = One.update action model.bottomCounter }

view :: T.Render _ Model _ Action
view send state _ _ =
    R.div'
        [ One.view (\a -> send (Top a)) state.topCounter [] []
        , One.view (send <<< Top) state.bottomCounter [] []
        , R.button [ RP.onClick \_ -> send Reset ]
                   [ R.text "RESET" ]
        ]

performAction :: T.PerformAction _ Model _ Action
performAction _ action =
    T.modifyState (update action)

initialState :: Model
initialState = init 0 0

spec :: T.Spec _ Model _ Action
spec = T.simpleSpec initialState performAction view

renderCounter :: forall eff. _ -> Eff (dom :: DOM.DOM | eff) R.ReactElement
renderCounter = R.render (R.createFactory (T.createClass spec) initialState)
