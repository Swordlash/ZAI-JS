-- | A big "Types" module is usually a bad practice, but we create such a module for simplicity
module Types where

import Data.Enum
import Prelude

import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Date (Date)
import Data.Date.Gen (genDate)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Fixed (Fixed, P100, fromNumber, toNumber)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.Traversable (sequence)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary, genericArbitrary)
import Test.QuickCheck.Gen (Gen, elements)
import Utils (formatDate)

----------------------------------------

newtype Author = Author { name :: String, surname :: String }

data Subject
  = Physics
  | Mathematics
  | ComputerScience
  | Chemistry
  | Geography
  | Economy
  | Law
  | Linguistics
  | Other

newtype URL         = URL String
newtype Title       = Title String
newtype Description = Description String
newtype IssueDate   = IssueDate Date
newtype Money = Money (Fixed P100)

data ModifyMode = On | Off

type BookRec =
  ( title       :: Title
  , description :: Description
  , imageUrl    :: URL
  , price       :: Money
  , author      :: Author
  , subject     :: Subject
  , issueDate   :: IssueDate
  , id          :: Int
  , modifyMode  :: ModifyMode
  )

newtype Book = Book (Record BookRec)
newtype Books = Books (Array Book)

derive instance Newtype Book _
derive newtype instance Eq Book

derive instance Newtype Books _
derive newtype instance Show Books

-------------- INSTANCES ---------------

arbName :: Gen String
arbName = elements $ NonEmptyArray 
  [ "John", "James", "Albert", "Josh", "Jack", "George", "Ralph", "Jane", "Amanda", "Carrie" ]

arbSurname :: Gen String
arbSurname = elements $ NonEmptyArray 
  [ "Newman", "Bond", "Einstein", "Bloch", "String", "Marlow", "Fiennes", "Chopin", "Charles", "Bradshaw" ]

twoPartGen :: forall a. (String -> a) -> Array String -> Array String -> Gen a
twoPartGen construct arbStart arbEnd = 
  map (construct <<< joinWith " ") $ sequence $ map (elements <<< NonEmptyArray) [arbStart, arbEnd]

-----------------------------------------

instance Arbitrary Title where
  arbitrary = twoPartGen Title
    [ "The short history of", "All about", "Advanced", "Elements of", "Basics of" ]
    [ "atoms", "dinosaurs", "languages", "international law", "monetary policy" ]

instance Show Title where
  show (Title t) = t

derive instance Eq Title
derive instance Ord Title

-----------------------------------------

instance Arbitrary Description where
  arbitrary = twoPartGen Description
    [ "Wonderful", "Nice", "Insightful", "Best selling", "Ingenious", "A hard-to-forget" ]
    [ "book", "coursebook for students", "thing to read", "read", "story" ]

instance Show Description where
  show (Description d) = d

derive instance Eq Description

-----------------------------------------

instance Arbitrary URL where
  arbitrary = do
    seed <- arbitrary :: Gen Int
    pure $ URL $ "https://picsum.photos/seed/" <> show seed <> "/128"

instance Show URL where
  show (URL u) = u

derive instance Eq URL

-----------------------------------------

instance Arbitrary Author where
  arbitrary = do
    name <- arbName
    surname <- arbSurname
    pure $ Author { name, surname }

instance Show Author where
  show (Author {name, surname}) = surname <> ", " <> name

derive instance Eq Author

-----------------------------------------

instance Arbitrary IssueDate where
  arbitrary = map IssueDate genDate

instance Show IssueDate where
  show (IssueDate d) = formatDate d

derive instance Eq IssueDate

-----------------------------------------

derive instance Generic Subject _

instance Arbitrary Subject where
  arbitrary = genericArbitrary

instance Show Subject where
  show = genericShow

derive instance Eq Subject
derive instance Ord Subject

instance Bounded Subject where
  bottom = genericBottom
  top = genericTop

instance Enum Subject where
  succ = genericSucc
  pred = genericPred

instance BoundedEnum Subject where
  toEnum = genericToEnum
  fromEnum = genericFromEnum
  cardinality = genericCardinality

-----------------------------------------

mkUnsafeMoney :: Number -> Money
mkUnsafeMoney = unsafePartial $ Money <<< fromJust <<< fromNumber

derive instance Newtype Money _

instance Arbitrary Money where
  arbitrary = map (mkUnsafeMoney <<< (_ * 100.0)) arbitrary

instance Show Money where
  show (Money m) = show $ toNumber m

derive instance Eq Money
derive instance Ord Money

-----------------------------------------

instance Show ModifyMode where
  show On  = "On"
  show Off = "Off"

instance Arbitrary ModifyMode where
  arbitrary = pure Off

derive instance Eq ModifyMode

-----------------------------------------

derive instance Generic Book _

instance Arbitrary Book where
  arbitrary = genericArbitrary

instance Show Book where
  show = genericShow