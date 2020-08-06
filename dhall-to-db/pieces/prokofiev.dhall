let Types = ./types.dhall

let c = ./composer.dhall "Prokofiev,_Sergey"

in    c
    ⫽ { works =
        [ c.concerto
            Types.violin
            "Violin_Concerto_No.1,_Op.19"
            [ "Andantino", "Scerzo. Vivacissimo", "Moderato. Allegro moderato" ]
        , c.concerto
            Types.violin
            "Violin_Concerto_No.2,_Op.63"
            [ "Allegro moderato", "Andante assai", "Allegro ben marcato" ]
        , Types.add_parts
            [ Types.solo_violin ⫽ { part_num = 1 }
            , Types.solo_violin ⫽ { part_num = 2 }
            ]
            ( c.piece
                "Sonata_for_2_Violins,_Op.56"
                [ "Andante cantabile"
                , "Allegro"
                , "Commodo (quasi allegretto)"
                , "Allegro con brio"
                ]
            )
        ,   c.sonata
              Types.violin
              "Violin_Sonata_No.1,_Op.80"
              [ "Andante assai"
              , "Allegro brusco"
              , "Andante"
              , "Allegrissimo - Andate assai, come prima"
              ]
          ⫽ { url = Some
                "https://en.wikipedia.org/wiki/Violin_Sonata_No._1_(Prokofiev)"
            }
        , c.sonata
            Types.violin
            "Violin_Sonata_No.2,_Op.94bis"
            [ "Moderato"
            , "Presto - Poco piu mosso del - Tempo I"
            , "Andante"
            , "Allegro con brio - Poco meno mosso - Tempo I - Poco meno mosso - Allegro con brio"
            ]
        , c.sonata
            Types.violin
            "Sonata_for_Solo_Violin,_Op.115"
            [ "Moderato"
            , "Andante dolce. Tema con variazioni"
            , "Con brio. Allegro precipitato"
            ]
        , c.sonata
            Types.cello
            "Cello_Sonata,_Op.119"
            [ "Andante grave - Moderato animato - Andante - Andante grave, come prima"
            , "Moderato - Andante dolce - Moderato primo"
            , "Allegro, ma non troppo"
            ]
        , c.concerto
            Types.cello
            "Sinfonia_concertante,_Op.125"
            [ "Andante", "Allegro giusto", "Andante con moto" ]
        , c.symphony
            "Symphony_No.1,_Op.25"
            [ "Allegro"
            , "Larghetto"
            , "Gavotte. Non troppo allegro"
            , "Finale. Molto vivace"
            ]
        , c.symphony
            "Symphony_No.5,_Op.100"
            [ "Andante", "Allegro marcato", "Adagio", "Allegro giocoso" ]
        , c.symphony
            "Symphony_No.7,_Op.131"
            [ "Moderato", "Allegretto", "Andante espressivo", "Vivace" ]
        ]
      }
