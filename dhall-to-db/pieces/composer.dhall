-- Works of a single composer

let types = ./types.dhall

let piece =
      λ(composer : Text) →
      λ(title : Text) →
      λ(movements : List Text) →
          { title
          , url = Some "https://imslp.org/wiki/${title}_(${composer})"
          , instrumentation = [] : List types.Part
          , movements
          }
        : types.Work

let symphony =
      λ(composer : Text) →
      λ(title : Text) →
      λ(movements : List Text) →
          { title
          , url = Some "https://imslp.org/wiki/${title}_(${composer})"
          , instrumentation = types.string_orchestra
          , movements
          }
        : types.Work

let quartet =
      λ(composer : Text) →
      λ(title : Text) →
      λ(movements : List Text) →
          { title
          , url = Some "https://imslp.org/wiki/${title}_(${composer})"
          , instrumentation = types.string_quartet
          , movements
          }
        : types.Work

let solo =
      λ(composer : Text) →
      λ(instrument : types.StringInstrument) →
      λ(title : Text) →
      λ(movements : List Text) →
          { title
          , url = Some "https://imslp.org/wiki/${title}_(${composer})"
          , instrumentation =
            [ { instrument, solo = types.Solo.Solo, part_num = 0 } ]
          , movements
          }
        : types.Work

let concerto =
      λ(composer : Text) →
      λ(instrument : types.StringInstrument) →
      λ(title : Text) →
      λ(movements : List Text) →
        let solo = solo composer instrument title movements

        in      solo
              ⫽ { instrumentation =
                    solo.instrumentation # types.string_orchestra
                }
            : types.Work

in  λ(composer : Text) →
      { name = composer
      , url = Some "https://imslp.org/wiki/Category:${composer}"
      , sonata = solo composer
      , concerto = concerto composer
      , symphony = symphony composer
      , quartet = quartet composer
      , piece = piece composer
      , works = [] : List types.Work
      }
