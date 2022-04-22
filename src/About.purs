module About where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.HTML (HTMLElement)

rootRef :: H.RefLabel
rootRef = H.RefLabel "about"

data Action = Initialize

about :: forall q i o m. MonadEffect m => H.Component q i o m
about = H.mkComponent 
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
  }

render :: forall s w i. s -> HH.HTML w i
render _ = HH.div [ HP.ref rootRef ] []

foreign import replaceContent :: HTMLElement -> Effect Unit

handleAction :: forall state action slots output m. MonadEffect m => Action -> H.HalogenM state action slots output m Unit
handleAction Initialize = H.getHTMLElementRef rootRef >>= traverse_ (H.liftEffect <<< replaceContent)
