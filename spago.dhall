{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "css"
  , "datetime"
  , "effect"
  , "enums"
  , "fixed-precision"
  , "foldable-traversable"
  , "formatters"
  , "halogen"
  , "halogen-css"
  , "integers"
  , "js-date"
  , "lists"
  , "maybe"
  , "newtype"
  , "numbers"
  , "partial"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "safe-coerce"
  , "strings"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
