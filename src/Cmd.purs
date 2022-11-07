module Cmd where

import Prelude (show, ($), (<$>), (<*>))

import Cmd.Domain (Widget, nameValue)
import Cmd.Errors (Errors(..))
import Cmd.Rule (Rule, applyRule)
import Cmd.Validate (validateWidget, validateRule)

import Data.Array (foldMap)
import Data.Validation.Semigroup (V, validation)

-- | The input/ouput widget type. We need this because ADTs, while great for type-safe logic,
-- | don't work well with JSON.
type InputWidget =
  { name :: String
  , paint :: String
  , size :: String
  , core :: String
  }

-- | Output state is the same as input
type OutputWidget = InputWidget

-- | The unvalidated input action 
type InputAction =
  { name :: String
  , value :: String
  }

-- | The unvalidated input rule
type InputRule = Array InputAction

-- | The unvalidated command input 
type Input =
  { rule :: InputRule
  , widget :: InputWidget
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
  { widget :: OutputWidget
  , errors :: Array String
  }

-- | Execute the command. This is the function called by the Wasm runtime.
execute :: Input -> Output
execute input =
  validation
    (outputErrors input)
    output
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
output :: ValidatedInput -> Output
output { rule, widget } =
  { widget: mkOutputWidget $ applyRule rule widget
  , errors: []
  }

-- | Convert modified widget to output type.
mkOutputWidget :: Widget -> OutputWidget
mkOutputWidget widget =
  { name: nameValue widget.name
  , paint: show widget.paint
  , size: show widget.size
  , core: show widget.core
  }

