module Cmd.Validate where

import Prelude (pure, (<>), (<*>), (<$>), ($))

import Cmd.Action (Action(..))
import Cmd.Domain (Color(..), Material(..), Name(..), Size(..), Widget, mkWidget)
import Cmd.Errors (Errors, mkError)

import Data.String.Common (trim)
import Data.Validation.Semigroup (V, andThen, invalid)

-- | Non-empty string validation
nonEmpty :: String -> String -> V Errors String
nonEmpty field s =
  case trim s of
    "" -> invalid $ mkError $ field <> " cannot be empty"
    value -> pure value

-- | Name validation
validateName :: String -> V Errors Name
validateName s =
  Name <$> nonEmpty "Name" s

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

