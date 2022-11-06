module Cmd.Action where

import Prelude (class Eq, class Show)

import Cmd.Domain (Name, Color, Material, Size, Widget)

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

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
applyAction :: Action -> Widget -> Widget
applyAction (ApplyName name) widget = widget { name = name }
applyAction (ApplyPaint paint) widget = widget { paint = paint }
applyAction (ApplyCore core) widget = widget { core = core }
applyAction (ApplySize size) widget = widget { size = size }

