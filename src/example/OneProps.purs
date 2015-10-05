module Example.OneProps where

import Prelude

import Control.Monad.Eff

import qualified Thermite as T
import qualified Thermite.Action as T

import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP

import qualified DOMImport as DOM

type Model = Int

data Action
    = Increment
    | Decrement

update :: Action -> Model -> Model
update Increment model =
    model + 1
update Decrement model =
    model - 1

view :: T.Render _ Model _ Action
view send state _ _ =
    R.div'
        [ R.button [ RP.onClick \_ -> send Decrement ] 
                   [ R.text "-" ]
        , R.div'   [ R.text (show state) ]
        , R.button [ RP.onClick \_ -> send Increment ] 
                   [ R.text "+" ]
        ]

performAction :: T.PerformAction _ Model _ Action
performAction props action =
    props.send action

spec :: T.Spec _ Model _ Action
spec = T.simpleSpec 0 performAction view

factory = R.createFactory (T.createClass spec) 
