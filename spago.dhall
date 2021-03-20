{ name = "purescript-astar"
, dependencies =
  [ "assert"
  , "matrices"
  , "ordered-collections"
  , "psci-support"
  ]
, license = "MIT"
, repository = "https://github.com/lfarroco/purescript-astar.git"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs"]
}
