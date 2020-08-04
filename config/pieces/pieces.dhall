-- All strings except for movement names should use underscores instead of spaces
-- All imslp urls need to be url-encoded by the importer
-- Titles should correspond to imslp urls if they exist

let map = https://prelude.dhall-lang.org/List/map

let Types = ./types.dhall

in    map
        Types.ComposerType
        Types.Composer
        (λ(c : Types.ComposerType) → c.(Types.Composer))
        [ ./beethoven.dhall
        , ./brahms.dhall
        , ./prokofiev.dhall
        , ./sibelius.dhall
        , ./ysaye.dhall
        ]
    : List Types.Composer
