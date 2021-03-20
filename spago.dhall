{ name = "purescript-astar"
, dependencies =
  [ "assert"
  , "matrices"
  , "ordered-collections"
  , "psci-support"
  ]
, license = "MIT"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs"]
}
