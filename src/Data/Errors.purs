module Data.Errors where

import Prelude (class Eq, class Semigroup, class Show, (<>), ($))

import Data.Generic.Rep (class Generic)
import Data.Monoid (class Monoid)
import Data.Show.Generic (genericShow)

-- | Defines validation errors.
newtype Errors = Errors (Array String)

derive instance eqErrors :: Eq Errors
derive instance genericErrors :: Generic Errors _

instance showErrors :: Show Errors where
  show = genericShow

instance semigroupErrors :: Semigroup Errors where
  append (Errors e1) (Errors e2) = Errors $ e1 <> e2

instance monoidErrors :: Monoid Errors where
  mempty = Errors []

-- | Error helper
mkError :: String -> Errors
mkError s =
  Errors [ s ]

