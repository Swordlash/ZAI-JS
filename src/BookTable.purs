module BookTable where

import Prelude

import Data.Array (modifyAt, sortBy)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Symbol (class IsSymbol)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)
import Record (modify)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy)
import Types (Book(..), BookRec, Books(..))
import Utils (cl)

type Input  = Books
type Output = Books

type State = 
  { books :: Books
  }

data Action 
  = ChangeIdx Int (Book -> Book)
  | Receive Books
  | SortBy (Book -> Book -> Ordering)

changeIdx :: forall lab a rTail. Cons lab a rTail BookRec => IsSymbol lab => Int -> Proxy lab -> (a -> a) -> Action
changeIdx idx p f = ChangeIdx idx (coerce (modify p f :: Record BookRec -> Record BookRec))

{-
type BookRec =
  ( title       :: Title
  , description :: Description
  , imageUrl    :: URL
  , price       :: Money
  , author      :: Author
  , subject     :: Subject
  , issueDate   :: IssueDate
  )
-}

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
  labels = [ "Title", "Author", "Issue Date", "Subject", "Description", "Image", "Price" ]
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
    ]

handleAction :: forall m slots. Action -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  Receive books -> H.modify_ _ { books = books }

  SortBy ord    -> H.modify_ (\st -> st { books = coerce (sortBy ord) st.books }) *> (H.gets _.books >>= H.raise)

  ChangeIdx idx f -> H.modify_ (\st -> st { books = coerce (modifyAt' idx f) st.books }) *> (H.gets _.books >>= H.raise)

  where modifyAt' idx f arr = fromMaybe arr $ modifyAt idx f arr 