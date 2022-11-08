module Cmd.Action where

import Prelude (class Eq, class Show, show)

import Cmd.Domain (Name, Color, Material, Size, Widget)

import Data.Monoid (class Monoid)
import Data.Semigroup (class Semigroup, (<>))

-- | Action defines transformations that can be made to a widget.
data Action
  = ApplyName Name
  | ApplyPaint Color
  | ApplyCore Material
  | ApplySize Size
  | Cons Action Action
  | Nil

derive instance eqAction :: Eq Action

instance showAction :: Show Action where
  show (ApplyName name) = "(ApplyName " <> (show name) <> ")"
  show (ApplyPaint color) = "(ApplyPaint " <> (show color) <> ")"
  show (ApplyCore material) = "(ApplyCore " <> (show material) <> ")"
  show (ApplySize size) = "(ApplySize " <> (show size) <> ")"
  show (Cons a1 a2) = "(Cons " <> (show a1) <> " " <> (show a2) <> ")"
  show Nil = "Nil"

instance semigroupAction :: Semigroup Action where
  append Nil Nil = Nil
  append a1 Nil = a1
  append Nil a2 = a2
  append a1 a2 = Cons a1 a2

instance monoidAction :: Monoid Action where
  mempty = Nil

-- | Apply an action to a widget.
applyAction :: Action -> Widget -> Widget
applyAction (ApplyName name) widget = widget { name = name }
applyAction (ApplyPaint paint) widget = widget { paint = paint }
applyAction (ApplyCore core) widget = widget { core = core }
applyAction (ApplySize size) widget = widget { size = size }
applyAction (Cons a1 a2) widget = applyAction a2 (applyAction a1 widget)
applyAction Nil widget = widget

