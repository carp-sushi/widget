{ name = "widget"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "either"
  , "prelude"
  , "strings"
  , "test-unit"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
