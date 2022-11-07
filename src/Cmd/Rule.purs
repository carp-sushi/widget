module Cmd.Rule where

import Prelude (class Eq, class Show)

import Cmd.Action (Action, applyAction)
import Cmd.Domain (Widget)

import Data.Array (foldr)
import Data.Generic.Rep (class Generic)
import Data.Monoid (class Monoid)
import Data.Semigroup (class Semigroup, (<>))
import Data.Show.Generic (genericShow)

-- | Rule is a sequence of actions.
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
applyRule :: Rule -> Widget -> Widget
applyRule (Rule actions) widget =
  foldr applyAction widget actions

-- | Create a rule with one action.
mkRule :: Action -> Rule
mkRule action =
  Rule [ action ]

