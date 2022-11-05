module Data.Rule where

import Prelude (class Eq, class Show)

import Data.Array (foldl)
import Data.Generic.Rep (class Generic)
import Data.Monoid (class Monoid)
import Data.Semigroup (class Semigroup, (<>))
import Data.Show.Generic (genericShow)

import Data.Action (Action, applyAction)
import Data.Widget (Widget)

-- | Defines a set of actions.
newtype Rule = Rule (Array Action)

derive instance eqRule :: Eq Rule
derive instance genericRule :: Generic Rule _

instance showRule :: Show Rule where
  show = genericShow

instance semigroupRule :: Semigroup Rule where
  append (Rule aa1) (Rule aa2) = Rule (aa1 <> aa2)

instance monoidRule :: Monoid Rule where
  mempty = Rule []

-- | Apply a rule (set of actions) to a widget.
applyRule :: Widget -> Rule -> Widget
applyRule widget (Rule actions) =
  foldl (\w a -> applyAction w a) widget actions

