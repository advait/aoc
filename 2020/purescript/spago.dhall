{ name = "aoc-2020-purescript"
, dependencies =
  [ "console"
  , "effect"
  , "maybe"
  , "psci-support"
  , "quickcheck"
  , "spec"
  , "spec-discovery"
  , "spec-quickcheck"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
