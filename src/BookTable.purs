module BookTable where

import Prelude

import CSS as CSS
import Control.Alternative (guard)
import Data.Enum (enumFromTo, fromEnum, toEnum)
import Data.Fixed (fromNumber)
import Data.Int as Int
import Data.JSDate as JSD
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (unwrap)
import Data.Number as Num
import Data.String (Pattern(..), drop, indexOf, take)
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Types (Author(..), Book(..), BookRec, Books, Description(..), IssueDate(..), ModifyMode(..), Money(..), Title(..), mkUnsafeMoney)
import Utils (cl, cls)

type Input  = Books
data Output 
  = ChangeId Int (Record BookRec -> Record BookRec)
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

  toTd  = toTd' <<< HH.text

  toTd' :: forall w i. HH.HTML w i -> HH.HTML w i
  toTd' = HH.td_ <<< pure

  toNodes (Book book) = 
    case book.modifyMode of
      Off -> 
        [ toTd $ show $ book.title
        , toTd $ show $ book.author
        , toTd $ show $ book.issueDate
        , toTd $ show $ book.subject
        , toTd $ show $ book.description
        , toTd' $ HH.img [ HP.src (show book.imageUrl) ]
        , toTd $ ("$" <> show book.price)
        , modifyColumn
        ]
      On -> 
        [ toTd' $ HH.textarea [ cl "form-control"
                              , HP.value (show book.title) 
                              , HE.onValueChange $ \str -> Raise $ ChangeId book.id (_ { title = Title str })
                              ]
        , toTd' $ HH.textarea [ cl "form-control"
                              , HP.value (show book.author) 
                              , HE.onValueChange $ \str -> Raise $ ChangeId book.id (_ { author = mkAuthor str })
                              ]
        , toTd' $ HH.input [ HP.type_ InputDate
                           , cl "form-control"
                           , HP.value $ show book.issueDate
                           , HE.onValueChange $ \str -> Raise $ ChangeId book.id ( _ { issueDate = mkIssueDate str })
                           ]
        , toTd' $ HH.select [ cl "form-select" 
                            , HE.onValueChange
                              \idx -> Raise $ ChangeId book.id 
                                      (_ { subject = unsafePartial $ fromJust $ Int.fromString idx >>= toEnum })
                            ] 
                            mkSubjectOptions
        , toTd' $ HH.textarea [ cl "form-control"
                              , HP.value (show book.description) 
                              , HE.onValueChange $ \str -> Raise $ ChangeId book.id (_ { description = Description str })
                              ]
        , toTd' $ HH.img [ HP.src (show book.imageUrl) ]
        , toTd' $ HH.input [ HP.type_ InputNumber
                           , cl "form-control"
                           , HP.value (show book.price) 
                           , HE.onValueChange $ \val -> Raise $ ChangeId book.id (\s -> s { price = parsePrice s.price val })
                           ]
        , modifyColumn     
        ]
    
    where
      mkAuthor :: String -> Author
      mkAuthor str = case indexOf (Pattern ",") str of
        Nothing -> Author { name: "NN", surname: str }
        Just idx -> Author { name: drop (idx+1) str, surname: take idx str }

      parsePrice :: Money -> String -> Money
      parsePrice m str = fromMaybe m $ map Money $ Num.fromString str >>= guardNegative >>= fromNumber
        where
          guardNegative :: Number -> Maybe Number
          guardNegative num = guard (num >= 0.0) *> pure num 

      mkSubjectOptions = flip map (enumFromTo bottom top) $ \option
        -> HH.option [ HP.value (show $ fromEnum option)
                     , HP.selected (option == book.subject) 
                     ] 
                     [ HH.text $ show option]

      mkIssueDate :: String -> IssueDate
      mkIssueDate str = IssueDate $ unsafePartial $ fromJust $ JSD.toDate $ unsafePerformEffect (JSD.parse str)

      modifyColumn = HH.td_
          [ HH.div 
            [ cls [ "form-check", "form-switch" ] ]
            [ HH.input [ cl "form-check-input"
                       , HP.type_ InputCheckbox
                       , HP.id "setModifyMode"
                       , HP.checked (book.modifyMode == On)
                       , HE.onChecked \checked -> Raise $ ChangeId book.id 
                          \b -> if checked then b { modifyMode = On } else b { modifyMode = Off }
                       ]
            , HH.label [ cl "form-check-label", HP.for "setModifyMode" ] [ HH.text "Modify" ]
            ]
          , HH.div [ cl "w-100", CSS.style $ CSS.height (CSS.px 5.0) ] [] 
          , HH.button [ cls [ "btn", "btn-danger"], HE.onClick \_ -> Raise (RemoveId book.id) ] [ HH.text "Remove" ]
          ]

handleAction :: forall m slots. Action -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  Receive books -> H.modify_ _ { books = books }
  Raise output  -> H.raise output