module Main where

import Prelude (Unit, discard, ($))

import Data.Cmd (CmdAction, CmdWidget, execute, mkCmdWidget)
import Data.Widget (Name(..), Color(..), Size(..), Material(..), mkWidget)
import Effect (Effect)
import Effect.Console (log, logShow)

rule :: Array CmdAction
rule =
  [ { name: "ApplyPaint", value: "Red" }
  , { name: "ApplySize", value: "Medium" }
  ]

widget :: CmdWidget
widget =
  mkCmdWidget $ mkWidget (Name "default") White Small Carbon

empty :: CmdWidget
empty =
  { name: "", paint: "", size: "", core: "" }

badRule :: Array CmdAction
badRule =
  [ { name: "ApplyCoating", value: "Wax" }
  , { name: "", value: "" }
  ]

badValue :: Array CmdAction
badValue =
  [ { name: "ApplySize", value: "X-Large" } ]

-- | Build and print a widget using the example actions.
main :: Effect Unit
main = do
  let input = { rule, widget }
  log "input:"
  logShow input
  log "output:"
  logShow $ execute input
  log "\n*** Error Cases ***\n"
  log "invalid widget:"
  logShow $ execute { rule: badRule, widget: empty }
  log "invalid action:"
  logShow $ execute { rule: badRule, widget }
  log "invalid action value:"
  logShow $ execute { rule: badValue, widget }
