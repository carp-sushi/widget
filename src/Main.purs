module Main where

import Prelude (Unit, discard, ($))

import Cmd (InputAction, InputWidget, execute)

import Effect (Effect)
import Effect.Console (log, logShow)

-- | Valid input actions.
actions :: Array InputAction
actions =
  [ { name: "ApplyPaint", value: "Red" }
  , { name: "ApplySize", value: "Medium" }
  , { name: "ApplyPaint", value: "Green" } -- Last action wins
  ]

-- | Valid input widget.
widget :: InputWidget
widget =
  { name: "Example"
  , paint: "White"
  , size: "Small"
  , core: "Carbon"
  }

-- | Demonstrate command execution.
main :: Effect Unit
main = do
  let input = { actions, widget }
  log "input:"
  logShow input
  log "output:"
  logShow $ execute input
