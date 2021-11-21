module Utils where

import Prelude

import Data.Date (Date)
import Data.DateTime (DateTime(..))
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List (fromFoldable)
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

cl :: forall r i. String -> IProp ( class :: String | r ) i
cl = HP.class_ <<< HH.ClassName

cls :: forall r i. Array String -> IProp ( class :: String | r) i
cls = HP.classes <<< map HH.ClassName

formatDate :: Date -> String
formatDate d =
  format (fromFoldable [DayOfMonth, Placeholder "-", MonthTwoDigits, Placeholder "-", YearFull]) (DateTime d bottom)