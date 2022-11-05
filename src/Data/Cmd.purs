module Data.Cmd where

import Prelude (show, ($), (<$>), (<*>))

import Data.Array (foldMap)
import Data.Validation.Semigroup (V, validation)

import Data.Errors (Errors(..))
import Data.Rule (Rule, applyRule)
import Data.Validate (validateWidget, validateRule)
import Data.Widget (Widget, nameValue)

-- | The input/ouput widget type. We need this because ADTs, while great for type-safe logic,
-- | don't work well with JSON.
type CmdWidget =
  { name :: String
  , paint :: String
  , size :: String
  , core :: String
  }

-- | The unvalidated input action 
type CmdAction =
  { name :: String
  , value :: String
  }

-- | The unvalidated command input 
type Input =
  { rule :: Array CmdAction
  , widget :: CmdWidget
  }

-- | The validated command input 
type ValidatedInput =
  { rule :: Rule
  , widget :: Widget
  }

-- | Validated input constructor.
mkValidated :: Rule -> Widget -> ValidatedInput
mkValidated rule widget =
  { rule, widget }

-- | The command output type.
type Output =
  { widget :: CmdWidget
  , errors :: Array String
  }

-- | Execute the command. This is the function called by the Wasm runtime.
execute :: Input -> Output
execute input =
  validation
    (outputErrors input)
    outputWidget
    (validateInput input)

-- | Validate input widget and rule.
validateInput :: Input -> V Errors ValidatedInput
validateInput { widget: w, rule: actions } =
  mkValidated
    <$> foldMap (\a -> validateRule a.name a.value) actions
    <*> validateWidget w.name w.paint w.size w.core

-- | Validation failure: add validation errors to the output.
outputErrors :: Input -> Errors -> Output
outputErrors input (Errors errors) =
  { widget: input.widget
  , errors
  }

-- | Validation success: apply the rule and add the updated widget to the output.
outputWidget :: ValidatedInput -> Output
outputWidget { rule, widget } =
  { widget: mkCmdWidget $ applyRule rule widget
  , errors: []
  }

-- | Convert modified widget to output type.
mkCmdWidget :: Widget -> CmdWidget
mkCmdWidget widget =
  { name: nameValue widget.name
  , paint: show widget.paint
  , size: show widget.size
  , core: show widget.core
  }

