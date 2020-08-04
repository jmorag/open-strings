let Types = ./types.dhall

let c = ./composer.dhall "Brahms,_Johannes"

in    c
    ⫽ { works =
        [ c.concerto
            Types.violin
            "Violin_Concerto,_Op.77"
            [ "Allegro non troppo", "Adagio", "Allegro giocoso, ma non troppo" ]
        , Types.add_parts
            [ Types.solo_cello ]
            ( c.concerto
                Types.violin
                "Concerto_for_Violin_and_Cello,_Op.102"
                [ "Allegro", "Andante", "Vivace non troppo" ]
            )
        ]
      }
