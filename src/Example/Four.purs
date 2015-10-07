module Example.Four where

import Prelude
import Data.Array
import Control.Plus (Plus)

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import qualified Example.Counter as Counter
import Example.Two (CounterSlot(..))
