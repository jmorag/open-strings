let map = https://prelude.dhall-lang.org/List/map

let Types = ./types.dhall

let Work = Types.Work

let c = ./composer.dhall "Ysaÿe,_Eugène"

let solo_sonatas =
      map
        Work
        Work
        ( λ(w : Work) →
              w
            ⫽ { url = Some
                  "https://imslp.org/wiki/6_Sonatas_for_Solo_Violin,_Op.27_(Ysaÿe,_Eugène)"
              }
        )
        [ c.sonata
            Types.violin
            "Solo Sonata No. 1, Op. 27 (A Joseph Szigeti)"
            [ "Grave. Lento assai"
            , "Fugato. Molto moderato"
            , "Allegretto poco scherzoso. Amabile"
            , "Finale con brio. Allegro fermo"
            ]
        , c.sonata
            Types.violin
            "Solo_Sonata_No._2,_Op._27_(A_Jacques_Thibaud)"
            [ "Obsession. Prélude. Poco vivace"
            , "Malinconia. Poco lento"
            , "Danse des ombres. Sarabande (lento)"
            , "Les furies. Allegro furioso"
            ]
        , c.sonata
            Types.violin
            "Solo_Sonata_No._3,_Op._27_(A_Georges_Enesco)"
            [ "Ballade. Lento molto sostenuto in modo di recitativo - Molto moderato quasi lento - Allegro in tempo giusto e con bravura"
            ]
        , c.sonata
            Types.violin
            "Solo_Sonata_No._4,_Op._27_(A_Fritz_Kreisler)"
            [ "Allemanda. Lento maestoso"
            , "Sarabande. Quasi lento"
            , "Finale. Presto ma non troppo"
            ]
        , c.sonata
            Types.violin
            "Solo_Sonata_No._5,_Op._27_(A_Mathieu_Crickboom)"
            [ "L'Aurore. Lento assai"
            , "Danse rustique. Allegro giocoso molto moderato"
            ]
        , c.sonata
            Types.violin
            "Solo_Sonata_No._6,_Op._27_(A_Manuel_Quiroga)"
            [ "Allegro giusto non troppo vivo - Allegretto poco scherzando - Allegro Tempo I"
            ]
        ]

in    c
    ⫽ { works =
            [ Types.add_parts
                [ Types.solo_violin ⫽ { part_num = 1 }
                , Types.solo_violin ⫽ { part_num = 2 }
                ]
                (c.symphony "Amitié,_Op.26" ([] : List Text))
            , c.sonata Types.violin "Poème_élégiaque,_Op.12" ([] : List Text)
            ]
          # solo_sonatas
      }
