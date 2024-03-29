-- instrumentation should be Set Part to guarantee uniqueness but
-- dhall doesn't support a Set type yet
-- https://github.com/dhall-lang/dhall-lang/issues/88

let Solo = < Solo | Tutti >

let StringInstrument = < Violin | Viola | Cello | Bass >

let Part = { instrument : StringInstrument, solo : Solo, part_num : Natural }

let violin = StringInstrument.Violin

let viola = StringInstrument.Viola

let cello = StringInstrument.Cello

let bass = StringInstrument.Bass

let solo_violin = { instrument = violin, solo = Solo.Solo, part_num = 0 }

let solo_viola = { instrument = viola, solo = Solo.Solo, part_num = 0 }

let solo_cello = { instrument = cello, solo = Solo.Solo, part_num = 0 }

let solo_bass = { instrument = bass, solo = Solo.Solo, part_num = 0 }

let Work =
      { title : Text
      , url : Optional Text
      , instrumentation : List Part
      , movements : List Text
      }

let add_parts =
      λ(p : List Part) →
      λ(w : Work) →
        w ⫽ { instrumentation = w.instrumentation # p }

let Piece
    : Type
    = Text → List Text → Work

let ComposerType
    : Type
    = { name : Text
      , url : Optional Text
      , sonata : StringInstrument → Piece
      , concerto : StringInstrument → Piece
      , symphony : Piece
      , quartet : Piece
      , piece : Piece
      , works : List Work
      }

in  { Solo
    , StringInstrument
    , violin
    , viola
    , cello
    , bass
    , solo_violin
    , solo_viola
    , solo_cello
    , solo_bass
    , Part
    , string_quartet =
      [ { instrument = violin, solo = Solo.Solo, part_num = 1 }
      , { instrument = violin, solo = Solo.Solo, part_num = 2 }
      , { instrument = viola, solo = Solo.Solo, part_num = 0 }
      , { instrument = cello, solo = Solo.Solo, part_num = 0 }
      ]
    , string_orchestra =
      [ { instrument = violin, solo = Solo.Tutti, part_num = 1 }
      , { instrument = violin, solo = Solo.Tutti, part_num = 2 }
      , { instrument = viola, solo = Solo.Tutti, part_num = 0 }
      , { instrument = cello, solo = Solo.Tutti, part_num = 0 }
      , { instrument = bass, solo = Solo.Tutti, part_num = 0 }
      ]
    , Work
    , Composer = { name : Text, url : Optional Text, works : List Work }
    , add_parts
    , ComposerType
    }
