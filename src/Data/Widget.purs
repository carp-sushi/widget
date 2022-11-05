module Data.Widget where

import Prelude (class Eq, class Show, (==))

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- | Core data type
type Widget =
  { name :: Name
  , paint :: Color
  , size :: Size
  , core :: Material
  }

-- | Widget constructor
mkWidget :: Name -> Color -> Size -> Material -> Widget
mkWidget name paint size core =
  { name
  , paint
  , size
  , core
  }

-- | Widget name type
newtype Name = Name String

instance eqName :: Eq Name where
  eq (Name n1) (Name n2) = n1 == n2

instance showName :: Show Name where
  show = nameValue

-- | Access the inner string of a name.
nameValue :: Name -> String
nameValue (Name value) = value

-- | Widget color options
data Color
  = White
  | Black
  | Red
  | Green
  | Blue
  | Clear

derive instance eqColor :: Eq Color
derive instance genericColor :: Generic Color _

instance showColor :: Show Color where
  show = genericShow

-- | Widget size options
data Size
  = Small
  | Medium
  | Large

derive instance eqSize :: Eq Size
derive instance genericSize :: Generic Size _

instance showSize :: Show Size where
  show = genericShow

-- | Widget material options
data Material
  = Carbon
  | Plastic
  | Aluminum
  | Steel

derive instance eqMaterial :: Eq Material
derive instance genericMaterial :: Generic Material _

instance showMaterial :: Show Material where
  show = genericShow

