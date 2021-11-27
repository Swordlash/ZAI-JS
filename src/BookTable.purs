module BookTable where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Types (Book(..), Books)
import Utils (cl)

type Input  = Books
data Output 
  = ChangeId Int (Book -> Book)
  | RemoveId Int

type State = 
  { books :: Books
  }

data Action 
  = Raise Output
  | Receive Books

bookTable :: forall q m. H.Component q Input Output m
bookTable = H.mkComponent
  { initialState: {books: _}
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction
                                 , receive = Just <<< Receive
                                 }
  }

render :: forall slots m. State -> H.ComponentHTML Action slots m
render {books} = HH.table [ cl "table" ] [ thead, tbody ]
  where
  labels = [ "Title", "Author", "Issue Date", "Subject", "Description", "Image", "Price", "Actions" ]
  thead = HH.thead_ $ pure $ HH.tr_ $ map (HH.th [ HP.scope HP.ScopeCol ] <<< pure <<< HH.text) labels
  tbody = HH.tbody_ $ map (HH.tr_ <<< toNodes) $ unwrap books

  toTd = HH.td_ <<< pure <<< HH.text

  toNodes (Book book) = 
    [ toTd $ show $ book.title
    , toTd $ show $ book.author
    , toTd $ show $ book.issueDate
    , toTd $ show $ book.subject
    , toTd $ show $ book.description
    , HH.td_ $ pure $ HH.img [ HP.src (show book.imageUrl) ]
    , toTd $ ("$" <> show book.price)
    , HH.td [ cl "row" ] 
      [ HH.button [ cl "btn", cl "btn-primary" ] [ HH.text "Modify item" ]
      , HH.div [ cl "w-100", CSS.style $ CSS.height (CSS.px 5.0) ] [] 
      , HH.button [ cl "btn", cl "btn-secondary", HE.onClick \_ -> Raise (RemoveId book.id) ] [ HH.text "Remove item" ]
      ]
    ]

handleAction :: forall m slots. Action -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  Receive books -> H.modify_ _ { books = books }
  Raise output  -> H.raise output