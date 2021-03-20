{ name = "purescript-astar"
, dependencies =
  [ "assert"
  , "matrices"
  , "ordered-collections"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs"]
}
