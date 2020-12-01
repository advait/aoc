{ name = "aoc-2020-purescript"
, dependencies =
  [ "console"
  , "effect"
  , "integers"
  , "maybe"
  , "psci-support"
  , "quickcheck"
  , "spec"
  , "spec-discovery"
  , "spec-quickcheck"
  , "undefined"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
