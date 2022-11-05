module Data.Action where

import Prelude (class Eq, class Show)

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import Data.Widget (Name, Color, Material, Size, Widget)

-- | Defines actions that may be peformed on a widget.
data Action
  = ApplyName Name
  | ApplyPaint Color
  | ApplyCore Material
  | ApplySize Size

derive instance eqAction :: Eq Action
derive instance genericAction :: Generic Action _

instance showAction :: Show Action where
  show = genericShow

-- | Apply an action to a widget.
applyAction :: Widget -> Action -> Widget
applyAction widget (ApplyName name) = widget { name = name }
applyAction widget (ApplyPaint paint) = widget { paint = paint }
applyAction widget (ApplyCore core) = widget { core = core }
applyAction widget (ApplySize size) = widget { size = size }

