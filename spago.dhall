{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "affjax"
  , "b64"
  , "biscotti-cookie"
  , "console"
  , "crypto"
  , "effect"
  , "envisage"
  , "form-urlencoded"
  , "http-methods"
  , "httpure"
  , "parsing"
  , "postgresql-client"
  , "psci-support"
  , "uri"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
