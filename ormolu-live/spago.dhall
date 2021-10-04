{ name = "ormolu-live"
, dependencies =
  [ "prelude"
  , "arrays"
  , "maybe"
  , "either"
  , "tuples"
  , "console"
  , "exceptions"
  , "effect"
  , "aff"
  , "aff-promise"
  , "flame"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
