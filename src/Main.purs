module Main where

import Prelude (Unit, discard, ($))

import Cmd (CmdAction, CmdWidget, execute)
import Effect (Effect)
import Effect.Console (log, logShow)

-- | Valid input rule.
rule :: Array CmdAction
rule =
  [ { name: "ApplyPaint", value: "Red" }
  , { name: "ApplySize", value: "Medium" }
  ]

-- | Valid input widget.
widget :: CmdWidget
widget =
  { name: "default"
  , paint: "White"
  , size: "Small"
  , core: "Carbon"
  }

-- | Demonstract command execution.
main :: Effect Unit
main = do
  let input = { rule, widget }
  log "input:"
  logShow input
  log "output:"
  logShow $ execute input
