module Main where

import BookTable
import Control.Alternative
import Control.Monad
import Prelude
import Safe.Coerce
import Unsafe.Coerce

import CSS as CSS
import Data.Array (cons, filter, sortBy, uncons)
import Data.Fixed (toNumber, fromNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Number as Num
import Data.Ordering (invert)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..), InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI) as HD
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (randomSample, randomSampleOne)
import Type.Proxy (Proxy(..))
import Types (Book(..), BookRec, Books(..), ModifyMode(..), Money(..), mkUnsafeMoney)
import Utils (cl, cls)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  HD.runUI component unit body

-------------------------------------------------------------

type Slots = ( bookTable :: forall q. H.Slot q Output Unit )

_bookTable = Proxy :: _ "bookTable"

type State = 
  { books :: Books 
  , view :: Books
  , sortTitleDescending :: Boolean
  , sortPriceDescending :: Boolean
  , fromPrice :: Money
  , toPrice   :: Money
  }

data Action
  = Initialize
  | Raised Output
  | SortTitleDescending Boolean
  | SortByTitle
  | SortPriceDescending Boolean
  | SortByPrice
  | FilterFromPrice String
  | FilterToPrice String
  | Filter
  | ClearFilter
  | AddNew

component :: forall q i o m. MonadEffect m => H.Component q i o m
component = H.mkComponent
  { initialState: const { books: Books []
                        , view: Books []
                        , sortTitleDescending: false
                        , sortPriceDescending: false 
                        , fromPrice: mkUnsafeMoney 0.0
                        , toPrice: mkUnsafeMoney 100.0
                        }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction
                                 , initialize = Just Initialize 
                                 }
  }
  where
    render :: State -> _
    render state = 
      HH.div [ cl "container" ] [ title, content ]
    
      where

      title   = HH.div [ cl "row" ] [ HH.h1_ [ HH.text "Bookstore" ] ]
      content = HH.div [ cl "row" ] [ menu, table ]

      menu = 
        HH.div [ cl "col-md-3 card" ] [ body ]

      options = 
        HH.ul [ cls [ "list-group", "list-group-flush"] ]
        [ HH.li [ cl "list-group-item" ] $ pure $ HH.div [ cl "row" ] []
        , HH.li [ cl "list-group-item" ] $ pure $ HH.div [ cl "row" ]
          [ HH.h3 [ cls [ "card-title", "col-12" ] ] [ HH.text "Sort" ]
          , HH.div [ cl "col-7" ] 
            $ pure 
            $ HH.button [ cls ["btn", "btn-primary" ]
                        , HP.type_ ButtonButton 
                        , HE.onClick \_ -> SortByTitle
                        ] 
                        [ HH.text "Sort by title" ] 
          , HH.div [ cl "col-3" ] $ pure $ HH.div [ cl "form-check" ] 
            [ HH.input [ cl "form-check-input"
                       , HP.type_ InputCheckbox
                       , HP.id "sortCheckbox" 
                       , HP.checked state.sortTitleDescending
                       , HE.onChecked SortTitleDescending
                       ]
            , HH.label [ cl "form-check-label", HP.for "sortCheckbox" ] [ HH.text "descending?" ]
            ]
          , HH.div [ cl "w-100", CSS.style $ CSS.height (CSS.px 5.0) ] [] 
          , HH.div [ cl "col-7" ] 
            $ pure 
            $ HH.button [ cls ["btn", "btn-primary" ]
                        , HP.type_ ButtonButton 
                        , HE.onClick \_ -> SortByPrice
                        ] 
                        [ HH.text "Sort by price" ] 
          , HH.div [ cl "col-3" ] $ pure $ HH.div [ cl "form-check" ] 
            [ HH.input [ cl "form-check-input"
                       , HP.type_ InputCheckbox
                       , HP.id "sortCheckbox2"
                       , HP.checked state.sortPriceDescending
                       , HE.onChecked SortPriceDescending
                       ]
            , HH.label [ cl "form-check-label", HP.for "sortCheckbox2" ] [ HH.text "descending?" ]
            ]
          ]
        , HH.li [ cl "list-group-item " ] $ pure $ HH.div [cl "row" ] 
          [ HH.h3 [ cls ["card-title",  "col-12"] ] [ HH.text "Filter" ]
          , HH.div [ cls ["mb-3", "row"] ] 
            [ HH.label [ cls [ "col-10", "col-form-label" ], HP.for "fromPrice" ] [ HH.text "From price:" ] 
            , HH.input [ cls [ "form-control", "col-2" ]
                     , HP.type_ InputNumber
                     , HP.id "fromPrice"
                     , HP.value (show $ toNumber $ unwrap state.fromPrice) 
                     , HE.onValueChange FilterFromPrice
                     ]
            ]
          , HH.div [ cls [ "mb-3", "row"] ] 
            [ HH.label [ cls [ "col-10", "col-form-label" ], HP.for "toPrice" ] [ HH.text "To price:" ] 
            , HH.input [ cls [ "form-control", "col-2" ]
                     , HP.type_ InputNumber
                     , HP.id "toPrice"
                     , HP.value (show $ toNumber $ unwrap state.toPrice) 
                     , HE.onValueChange FilterToPrice
                     ]
            ]
          , HH.div [ cl "w-100", CSS.style $ CSS.height (CSS.px 5.0) ] [] 
          , HH.button [ cls ["btn", "btn-primary" ]
                      , HP.type_ ButtonButton 
                      , HE.onClick \_ -> Filter
                      ] 
                      [ HH.text "Filter" ]
          , HH.div [ cl "w-100", CSS.style $ CSS.height (CSS.px 5.0) ] [] 
          , HH.button [ cls ["btn", "btn-secondary" ]
                      , HP.type_ ButtonButton 
                      , HE.onClick \_ -> ClearFilter
                      ] 
                      [ HH.text "Clear filter" ]
          ]
        , HH.li [ cl "list-group-item " ] 
            $ pure 
            $ HH.div [ cl "row" ]
            $ pure
            $ HH.button [ cls ["btn", "btn-info" ]
                        , HP.type_ ButtonButton 
                        , HE.onClick \_ -> AddNew
                        ] 
                        [ HH.text "Add new item" ]
        ]
      
      body = 
        HH.div [ cl "card-body" ]
        $ [ HH.h2 [ cl "card-title" ] [ HH.text "Menu" ]
          , options
          ]

      table =
        HH.div [ cl "col-md-9" ] $ pure $ HH.slot _bookTable unit bookTable state.view Raised

    handleOutput :: forall output. Output -> H.HalogenM State Action Slots output m Unit
    handleOutput = case _ of
      RemoveId id -> H.modify_ \st -> st { books = removeId st.books, view = removeId st.view }
        where 
          removeId :: Books -> Books
          removeId (Books books) = Books $ filter (\(Book b) -> b.id /= id) $ books

      ChangeId id f -> H.modify_ \st -> st { books = changeid st.books, view = changeid st.view }
        where
          changeid :: Books -> Books
          changeid (Books bs) = case uncons bs of
            Nothing -> Books []
            Just { head: b@(Book br), tail } ->
              if br.id == id 
                then Books $ cons (Book $ f br) tail
                else Books $ cons b $ unwrap $ changeid $ Books tail

    handleAction :: forall output. Action -> H.HalogenM State Action Slots output m Unit
    handleAction = case _ of
      Initialize -> do
        sampleData <- liftEffect $ Books <$> randomSample arbitrary
        H.modify_ _ { books = sampleData, view = sampleData }
      Raised output -> handleOutput output
      SortTitleDescending b -> H.modify_ _ { sortTitleDescending = b }
      SortByTitle -> do
        {sortTitleDescending} <- H.get
        let cmp = invertCmp sortTitleDescending (comparing _.title)
        H.modify_ \st -> st { view = wrap $ sortBy (coerce' cmp) $ unwrap st.view }
      SortPriceDescending b -> H.modify_ _ { sortPriceDescending = b }
      SortByPrice -> do
        {sortPriceDescending} <- H.get
        let cmp = invertCmp sortPriceDescending (comparing _.price)
        H.modify_ \st -> st { view = wrap $ sortBy (coerce' cmp) $ unwrap st.view }

      FilterFromPrice str -> 
        for_ (Num.fromString str >>= fromNumber) \fromPrice -> H.modify_ _ { fromPrice = Money fromPrice }
      FilterToPrice str -> 
        for_ (Num.fromString str >>= fromNumber) \fromPrice -> H.modify_ _ { toPrice = Money fromPrice }
      Filter -> do
        {fromPrice, toPrice} <- H.get
        H.modify_ \st -> st { view = wrap $ filter (\(Book book) -> book.price >= fromPrice && book.price <= toPrice) $ unwrap st.books }
      ClearFilter -> do
        H.modify_ _ { fromPrice = mkUnsafeMoney 0.0, toPrice = mkUnsafeMoney 100.0 }
        handleAction Filter

      AddNew -> do
        defBook <- map (\(Book b) -> Book b { modifyMode = On }) $ liftEffect $ randomSampleOne arbitrary
        H.modify_ \st -> st { books = wrap $ cons defBook $ unwrap st.books
                            , view  = wrap $ cons defBook $ unwrap st.view
                            }

      where
        invertCmp :: forall a. Boolean -> (a -> a -> Ordering) -> (a -> a -> Ordering)
        invertCmp false f = f
        invertCmp true  f = \x y -> invert $ f x y

        coerce' :: (Record BookRec -> Record BookRec -> Ordering) -> (Book -> Book -> Ordering)
        coerce' = coerce

