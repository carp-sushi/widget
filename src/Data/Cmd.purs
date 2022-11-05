module Data.Cmd where

import Prelude (pure, show, ($))

import Data.Array (foldMap)
import Data.Either (Either(..))
import Data.Monoid (mempty)
import Data.Validation.Semigroup (V(..), invalid, validation, andThen)

import Data.Rule (Rule, applyRule)
import Data.Validate (Errors(..), isEmpty, validateWidget, validateRule)
import Data.Widget (Widget, nameValue)

-- | The unvalidated input widget type
type CmdWidget =
  { name :: String
  , paint :: String
  , size :: String
  , core :: String
  }

-- | Convert modified widget to output type.
mkCmdWidget :: Widget -> CmdWidget
mkCmdWidget widget =
  { name: nameValue widget.name
  , paint: show widget.paint
  , size: show widget.size
  , core: show widget.core
  }

-- | The unvalidated input action type
type CmdAction =
  { name :: String
  , value :: String
  }

-- | The command input type
type Input =
  { rule :: Array CmdAction
  , widget :: CmdWidget
  }

-- | The command output type.
type Output =
  { widget :: CmdWidget
  , errors :: Array String
  }

-- | Execute the command.
execute :: Input -> Output
execute input =
  validation
    (outputErrors input)
    outputWidget
    (validateInput input)

-- | Validate input and apply all actions to a widget or return errors.
validateInput :: Input -> V Errors Widget
validateInput { widget: w, rule: rs } =
  validateWidget w.name w.paint w.size w.core `andThen` \widget ->
    if isEmpty errors then
      pure $ applyRule widget (getRule result)
    else
      invalid errors
  where
  result = foldMap (\r -> validateRule r.name r.value) rs
  errors = getErrors result

-- | Helper for folding validated rules into a single rule.
getRule :: V Errors Rule -> Rule
getRule (V (Left _)) = mempty
getRule (V (Right rule)) = rule

-- | Collect action validation errors.
getErrors :: V Errors Rule -> Errors
getErrors (V (Left errors)) = errors
getErrors (V (Right _)) = mempty

-- | Validation failure: add validation errors to the output.
outputErrors :: Input -> Errors -> Output
outputErrors input (Errors errors) =
  { widget: input.widget
  , errors
  }

-- | Validation success: add the updated widget to the output.
outputWidget :: Widget -> Output
outputWidget widget =
  { widget: mkCmdWidget widget
  , errors: []
  }

