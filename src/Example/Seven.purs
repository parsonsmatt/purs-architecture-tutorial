module Example.Seven where

import Prelude

import Control.Plus (Plus)
import Data.Functor.Coproduct
import Data.Functor (($>))
import Data.Array
import Data.Generic

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

type StateP =
  { itemArray :: Array Int
  , nextID :: Int
  }

initialStateP :: StateP
initialStateP =
  { itemArray: []
  , nextID: 0
  }

data QueryP a
  = AddItem a
  | RemItem a

newtype Slot = Slot Int

derive instance genericSlot :: Generic Slot

instance ordSlot :: Ord Slot where
  compare = gCompare

instance eqSlot :: Eq Slot where
  eq = gEq

makeList :: forall g p s f. (Plus g)
         => Component s f g
         -> s
         -> Component (State s f g) (Query f) g
makeList comp initState = parentComponent render eval
  where
    render state =
      H.div_
        [ H.button [ E.onClick $ E.input_ AddItem ]
                   [ H.text "+" ]
        , H.button [ E.onClick $ E.input_ RemItem ]
                   [ H.text "-" ]
        , H.ul_ (map (\i -> H.slot (Slot i) (initComp comp initState)) state.itemArray)
        ]

    initComp :: Component s f g -> s -> Unit -> { component :: _, initialState :: _ }
    initComp c s _ = {component: c, initialState: s}

    eval :: EvalParent QueryP StateP s QueryP f g Slot
    eval (AddItem next) = modify addItem $> next
    eval (RemItem next) = modify remItem $> next

addItem :: StateP -> StateP
addItem state =
  state { itemArray = state.nextID : state.itemArray
        , nextID = state.nextID + 1
        }

remItem :: StateP -> StateP
remItem state = 
  state { itemArray = drop 1 state.itemArray }

type State s f g =
  InstalledState StateP s QueryP f g Slot

type Query f =
  Coproduct QueryP (ChildF Slot f)

initialState :: forall s f g p. State s f g
initialState = installedState initialStateP
