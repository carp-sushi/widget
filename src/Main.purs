module Main where

import Prelude (Unit, discard, ($))

import Data.Cmd (CmdAction, CmdWidget, execute, mkCmdWidget)
import Data.Widget (Name(..), Color(..), Size(..), Material(..), mkWidget)
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
  mkCmdWidget $ mkWidget (Name "default") White Small Carbon

-- | Demonstract command execution.
main :: Effect Unit
main = do
  let input = { rule, widget }
  log "input:"
  logShow input
  log "output:"
  logShow $ execute input
