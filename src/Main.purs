module Main where

import Prelude

import BookTable (bookTable)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (Component, defaultEval, mkComponent, mkEval) as H
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI) as HD
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (randomSample)
import Type.Proxy (Proxy(..))
import Types (Books(..))
import Utils (cl)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  HD.runUI component unit body

-------------------------------------------------------------

type Slots = ( bookTable :: forall q. H.Slot q Books Unit )

type State = { books :: Books }

data Action
  = Initialize
  | Raised Books

component :: forall q i o m. MonadEffect m => H.Component q i o m
component = H.mkComponent
  { initialState: const { books: Books [] }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction
                                 , initialize = Just Initialize 
                                 }
  }
  where
    render state = 
      HH.div [ cl "container" ]
      $ pure
      $ HH.slot (Proxy :: Proxy "bookTable") unit bookTable state.books Raised

    handleAction = case _ of
      Initialize -> do
        sampleData <- liftEffect $ Books <$> randomSample arbitrary
        H.modify_ _ { books = sampleData }
      Raised books -> H.modify_ _ { books = books }

