module Example.Five where

import Prelude

import Data.Foldable (mconcat)
import Data.Either (either)
import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff
import Control.Monad.Free (liftFI)
import Data.Foreign.Class (readProp)

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import Network.HTTP.Affjax (AJAX())
import qualified Network.HTTP.Affjax as AJ

type State =
  { topic :: String
  , gifUrl :: String
  }

initialState :: State
initialState = { topic: "cats", gifUrl: "" }

data Input a
  = RequestMore a

type GifEffects eff = HalogenEffects (ajax :: AJAX | eff)

giphyKey :: String
giphyKey = "dc6zaTOxFJmzC"

giphyUrl :: String
giphyUrl = "https://api.giphy.com/v1/gifs/random"

giphyRequestUrl :: String -> String
giphyRequestUrl topic = mconcat
  [ giphyUrl
  , "?tag=", topic
  , "&api_key=", giphyKey
  ]

ui :: Component State Input (Aff (GifEffects ()))
ui = component render eval
  where
    render :: Render State Input
    render state =
      H.div_
        [ H.h2_ [ H.text state.topic ]
        , H.button [ E.onClick $ E.input_ RequestMore ]
                   [ H.text "Moar!!" ]
        , H.img [ P.src state.gifUrl ]
        ]

    eval :: Eval Input State Input (Aff (GifEffects ()))
    eval (RequestMore a) = do
      state <- get
      newGifUrlFn <- liftFI (fetchGif state.topic)
      modify \s -> s { gifUrl = newGifUrlFn s.gifUrl }
      pure a

fetchGif :: forall eff. String -> Aff (ajax :: AJAX | eff) (String -> String)
fetchGif topic = do
    result <- AJ.get (giphyRequestUrl topic)
    let url = readProp "data" result.response >>= readProp "image_url"
    pure (either (flip const) const url)
