module Cmd where

import Prelude (show, ($), (<$>), (<*>))

import Cmd.Domain (Widget)
import Cmd.Errors (Errors(..))
import Cmd.Action (Action, applyAction)
import Cmd.Validate (validateWidget, validateAction)

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

-- | The unvalidated command input 
type Input =
  { actions :: Array InputAction
  , widget :: InputWidget
  }

-- | The validated command input 
type ValidatedInput =
  { action :: Action
  , widget :: Widget
  }

-- | Validated input constructor.
mkValidated :: Action -> Widget -> ValidatedInput
mkValidated action widget =
  { action, widget }

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

-- | Validate input widget and actions.
validateInput :: Input -> V Errors ValidatedInput
validateInput { widget: w, actions } =
  mkValidated
    <$> foldMap (\a -> validateAction a.name a.value) actions
    <*> validateWidget w.name w.paint w.size w.core

-- | Validation failure: add validation errors to the output.
outputErrors :: Input -> Errors -> Output
outputErrors input (Errors errors) =
  { widget: input.widget
  , errors
  }

-- | Validation success: apply the action and add the updated widget to the output.
output :: ValidatedInput -> Output
output { action, widget } =
  { widget: mkOutput $ applyAction action widget
  , errors: []
  }
  where
    mkOutput w =
      { name: show w.name
      , paint: show w.paint
      , size: show w.size
      , core: show w.core
      }

