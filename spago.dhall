{ name = "widget"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "prelude"
  , "strings"
  , "test-unit"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
