{ name = "purescript-astar"
, dependencies =
  [ "assert"
  , "matrices"
  , "ordered-collections"
  , "arrays"
  , "console"
  , "effect"
  , "integers"
  , "maybe"
  , "numbers"
  , "prelude"
  , "tuples"
  ]
, license = "MIT"
, repository = "https://github.com/lfarroco/purescript-astar.git"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs"]
}
