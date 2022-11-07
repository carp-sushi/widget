module Main where

import Prelude (Unit, discard, ($))

import Cmd (InputRule, InputWidget, execute)

import Effect (Effect)
import Effect.Console (log, logShow)

-- | Valid input rule.
rule :: InputRule
rule =
  [ { name: "ApplyPaint", value: "Red" }
  , { name: "ApplySize", value: "Medium" }
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
  let input = { rule, widget }
  log "input:"
  logShow input
  log "output:"
  logShow $ execute input
