{ name = "astar"
, dependencies =
  [ "assert"
  , "console"
  , "effect"
  , "matrices"
  , "ordered-collections"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
