module Data.Validate where

import Prelude (class Eq, class Semigroup, class Show, pure, (<>), (<*>), (<$>), ($))

import Data.Array (null)
import Data.Generic.Rep (class Generic)
import Data.Monoid (class Monoid)
import Data.Show.Generic (genericShow)
import Data.String.Common (trim)
import Data.Validation.Semigroup (V, andThen, invalid)

import Data.Action (Action(..))
import Data.Rule (Rule(..))
import Data.Widget (Color(..), Material(..), Name(..), Size(..), Widget, mkWidget)

-- | Validation error type
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

-- | Check that error array is empty.
isEmpty :: Errors -> Boolean
isEmpty (Errors xs) = null xs

-- | Non-empty string validation
nonEmpty :: String -> String -> V Errors String
nonEmpty field s =
  case trim s of
    "" -> invalid $ mkError $ field <> " cannot be empty"
    value -> pure value

-- | Name validation
validateName :: String -> V Errors Name
validateName s =
  nonEmpty "Name" s `andThen` \value -> pure $ Name value

-- | Color validation 
validateColor :: String -> V Errors Color
validateColor s =
  nonEmpty "Color" s `andThen`
    case _ of
      "White" -> pure White
      "Black" -> pure Black
      "Red" -> pure Red
      "Green" -> pure Green
      "Blue" -> pure Blue
      "Clear" -> pure Clear
      _ -> invalid $ mkError $ "Invalid color: " <> s

-- | Size validation
validateSize :: String -> V Errors Size
validateSize s =
  nonEmpty "Size" s `andThen`
    case _ of
      "Small" -> pure Small
      "Medium" -> pure Medium
      "Large" -> pure Large
      _ -> invalid $ mkError $ "Invalid size: " <> s

-- | Material validation
validateMaterial :: String -> V Errors Material
validateMaterial s =
  nonEmpty "Material" s `andThen`
    case _ of
      "Carbon" -> pure Carbon
      "Plastic" -> pure Plastic
      "Aluminum" -> pure Aluminum
      "Steel" -> pure Steel
      _ -> invalid $ mkError $ "Invalid material: " <> s

-- | Widget validation
validateWidget :: String -> String -> String -> String -> V Errors Widget
validateWidget name paint size core =
  mkWidget
    <$> validateName name
    <*> validateColor paint
    <*> validateSize size
    <*> validateMaterial core

-- | Make an action from strings.
validateAction :: String -> String -> V Errors Action
validateAction name value =
  nonEmpty "Action name" name `andThen`
    case _ of
      "ApplyName" -> ApplyName <$> validateName value
      "ApplyPaint" -> ApplyPaint <$> validateColor value
      "ApplyCore" -> ApplyCore <$> validateMaterial value
      "ApplySize" -> ApplySize <$> validateSize value
      _ -> invalid $ mkError $ "Invalid action: " <> name

-- | Make a rule from validating a single action.
validateRule :: String -> String -> V Errors Rule
validateRule name value =
  validateAction name value `andThen` \action ->
    pure $ Rule [ action ]

