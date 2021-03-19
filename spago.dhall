{ name = "astar"
, dependencies =
  [ "console", "effect", "matrices", "ordered-collections", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
