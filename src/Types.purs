-- | A big "Types" module is usually a bad practice, but we create such a module for simplicity
module Types where

import Prelude

import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Date (Date)
import Data.Date.Gen (genDate)
import Data.Fixed (Fixed, P100, fromNumber, toNumber)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.Traversable (sequence)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary, genericArbitrary)
import Test.QuickCheck.Gen (Gen, elements)

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

newtype Book = Book
  { title       :: Title
  , description :: Description
  , imageUrl    :: URL
  , price       :: Money
  , author      :: Author
  , subject     :: Subject
  , issueDate   :: IssueDate
  }

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

-----------------------------------------

instance Arbitrary Description where
  arbitrary = twoPartGen Description
    [ "Wonderful", "Nice", "Insightful", "Best selling", "Ingenious" ]
    [ "book", "coursebook for students", "thing to read", "read" ]

instance Show Description where
  show (Description d) = d

-----------------------------------------

instance Arbitrary URL where
  arbitrary = pure $ URL "https://picsum.photos/256"

instance Show URL where
  show (URL u) = u

-----------------------------------------

instance Arbitrary Author where
  arbitrary = do
    name <- arbName
    surname <- arbSurname
    pure $ Author { name, surname }

instance Show Author where
  show (Author {name, surname}) = surname <> ", " <> name

-----------------------------------------

instance Arbitrary IssueDate where
  arbitrary = map IssueDate genDate

instance Show IssueDate where
  show (IssueDate d) = show d

-----------------------------------------

derive instance Generic Subject _

instance Arbitrary Subject where
  arbitrary = genericArbitrary

instance Show Subject where
  show = genericShow

-----------------------------------------

instance Arbitrary Money where
  arbitrary = map (unsafePartial $ Money <<< fromJust <<< fromNumber <<< (_ * 100.0)) arbitrary

instance Show Money where
  show (Money m) = show $ toNumber m

-----------------------------------------

derive instance Generic Book _

instance Arbitrary Book where
  arbitrary = genericArbitrary

instance Show Book where
  show = genericShow