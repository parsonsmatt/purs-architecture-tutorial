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

listUI :: forall g p s' f'. (Functor g)
       => ParentComponent StateP s' QueryP f' g Slot p
listUI = component render eval
  where
    render :: Render StateP QueryP Slot
    render state =
      H.div_
        [ H.button [ E.onClick $ E.input_ AddItem ]
                   [ H.text "+" ]
        , H.button [ E.onClick $ E.input_ RemItem ]
                   [ H.text "-" ]
        , H.ul_ (map (H.slot <<< Slot) state.itemArray)
        ]

    eval :: EvalP QueryP StateP s' QueryP f' g Slot p
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

type State s f g p =
  InstalledState StateP s QueryP f g Slot p

type Query f =
  Coproduct QueryP (ChildF Slot f)

initialState :: forall s f g p. State s f g p
initialState = installedState initialStateP

makeList :: forall s f g p. (Plus g)
         => Component s f g p
         -> s
         -> Component (State s f g p) (Query f) g p
makeList comp state = install listUI mkChild
  where
    mkChild (Slot _) = createChild comp state
