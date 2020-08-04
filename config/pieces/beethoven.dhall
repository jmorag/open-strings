let Types = ./types.dhall

let c = ./composer.dhall "Beethoven,_Ludwig_van"

in    c
    ⫽ { works =
        [ c.sonata
            Types.violin
            "Violin_Sonata_No.1,_Op.12_No.1"
            [ "Allegro con brio"
            , "Tema con variazioni. Andante con moto"
            , "Rondo - Allegro"
            ]
        , c.sonata
            Types.violin
            "Violin_Sonata_No.2,_Op.12_No.2"
            [ "Allegro vivace"
            , "Andante, più tosto Allegretto"
            , "Allegro piacevole"
            ]
        , c.sonata
            Types.violin
            "Violin_Sonata_No.3,_Op.12_No.3"
            [ "Allegro con spirito"
            , "Adagio con molt' espressione"
            , "Rondo. Allegro molto"
            ]
        , c.sonata
            Types.violin
            "Violin_Sonata_No.4,_Op.23"
            [ "Presto", "Andante scherzoso, più Allegretto", "Allegro molto" ]
        , c.sonata
            Types.violin
            "Violin_Sonata_No.5,_Op.24"
            [ "Allegro"
            , "Adagio molto espressivo"
            , "Scherzo. Allegro molto - Trio"
            , "Rondo. Allegro ma non troppo"
            ]
        , c.sonata
            Types.violin
            "Violin_Sonata_No.6,_Op.30_No.1"
            [ "Allegro"
            , "Adagio molto espressivo"
            , "Allegretto con variazioni"
            ]
        , c.sonata
            Types.violin
            "Violin_Sonata_No.7,_Op.30_No.2"
            [ "Allegro con brio"
            , "Adagio cantabile"
            , "Scherzo. Allegro - Trio"
            , "Finale. Allegro"
            ]
        , c.sonata
            Types.violin
            "Violin_Sonata_No.8,_Op.30_No.3"
            [ "Allegro assai"
            , "Tempo di Minuetto, ma molto moderato e grazioso"
            , "Allegro vivace"
            ]
        , c.sonata
            Types.violin
            "Violin_Sonata_No.9,_Op.47"
            [ "Adagio sostenuto - Presto"
            , "Andante con variazioni"
            , "Finale. Presto"
            ]
        , c.sonata
            Types.violin
            "Violin_Sonata_No.10,_Op.96"
            [ "Allegro moderato"
            , "Adagio espressivo"
            , "Scherzo. Allegro - Trio"
            , "Poco Allegretto"
            ]
        , c.concerto
            Types.violin
            "Violin_Concerto_in_D_major,_Op.61"
            [ "Allegro ma non troppo", "Larghetto", "Rondo. Allegro" ]
        , c.concerto Types.violin "Romance_in_G_major,_Op.40" ([] : List Text)
        , c.concerto Types.violin "Romance_in_F_major,_Op.50" ([] : List Text)
        , Types.add_parts
            [ Types.solo_cello ]
            ( c.concerto
                Types.violin
                "Triple_Concerto,_Op.56"
                [ "Allegro", "Largo", "Rondo alla Polacca" ]
            )
        , c.symphony
            "Piano_Concerto_No.1,_Op.15"
            [ "Allegro con brio", "Largo", "Rondo. Allegro" ]
        , c.symphony
            "Piano_Concerto_No.2,_Op.19"
            [ "Allegro con brio", "Adagio", "Rondo. Molto Allegro" ]
        , c.symphony
            "Piano_Concerto_No.3,_Op.37"
            [ "Allegro con brio", "Largo", "Rondo. Allegro - Presto" ]
        , c.symphony
            "Piano_Concerto_No.4,_Op.58"
            [ "Allegro moderato", "Andante con moto", "Rondo. Vivace" ]
        , c.symphony
            "Piano_Concerto_No.5,_Op.73"
            [ "Allegro", "Andante un pocco moto", "Rondo. Allegro" ]
        , c.symphony
            "Symphony_No.1,_Op.21"
            [ "Adagio molto - Allegro con brio"
            , "Andante cantabile con moto"
            , "Minuet. Allegro molto e vivace - Trio"
            , "Finale. Adagio - Allegro molto e vivace"
            ]
        , c.symphony
            "Symphony_No.2,_Op.36"
            [ "Adagio molto - Allegro con brio"
            , "Larghetto"
            , "Scherzo. Allegro - Trio"
            , "Allegro molto"
            ]
        , c.symphony
            "Symphony_No.3,_Op.55"
            [ "Allegro con brio"
            , "Marcia funebre. Adagio assai"
            , "Scherzo. Allegro vivace - Trio"
            , "Finale. Allegro molto"
            ]
        , c.symphony
            "Symphony_No.4,_Op.60"
            [ "Adagio - Allegro vivace"
            , "Adagio"
            , "Allegro vivace"
            , "Allegro ma non troppo"
            ]
        , c.symphony
            "Symphony_No.5,_Op.67"
            [ "Allegro con brio"
            , "Andante con moto"
            , "Scherzo. Allegro - Trio"
            , "Allegro"
            ]
        , c.symphony
            "Symphony_No.6,_Op.68"
            [ "Erwachen heiterer Empfindungen bei der Ankunft auf dem Lande. Allegro ma non troppo"
            , "Scene am Bach. Andante molto moto"
            , "Lustiges Zusammensein der Landleute. Allegro"
            , "Gewitter. Sturm. Allegro"
            , "Hirtengesang. Frohe und dankbare Gefühle nach dem Sturm. Allegretto"
            ]
        , c.symphony
            "Symphony_No.7,_Op.92"
            [ "Poco sostenuto - Vivace"
            , "Allegretto"
            , "Presto"
            , "Allegro con brio"
            ]
        , c.symphony
            "Symphony_No.8,_Op.93"
            [ "Allegro vivace e con brio"
            , "Allegretto scherzando"
            , "Tempo di Menuetto"
            , "Allegro vivace"
            ]
        , c.symphony
            "Symphony_No.9,_Op.125"
            [ "Allegro ma non troppo, un poco maestoso"
            , "Scherzo. Molto vivace - Presto"
            , "Adagio molto e cantabile"
            , "Presto (D minor) - Allegro assai (D major); Allegro molto assai (Alla marcia) (B♭ major); Andante maestoso (G major) - Adagio ma non troppo, ma divoto (G minor); Allegro energico, sempre ben marcato - Allegro ma non tanto - Pressitissmo (D major)"
            ]
        , c.quartet
            "String_Quartet_No.1,_Op.18_No.1"
            [ "Allegro con brio"
            , "Adagio affettuoso ed appasionato"
            , "Scherzo. Allegro molto"
            , "Allegro"
            ]
        , c.quartet
            "String_Quartet_No.2,_Op.18_No.2"
            [ "Allegro"
            , "Adagio cantabile"
            , "Scherzo. Allegro"
            , "Allegro molto, quasi Presto"
            ]
        , c.quartet
            "String_Quartet_No.3,_Op.18_No.3"
            [ "Allegro", "Adagio con moto", "Allegro", "Presto" ]
        , c.quartet
            "String_Quartet_No.4,_Op.18_No.4"
            [ "Allegro, ma non tanto"
            , "Scherzo. Andante scherzoso quasi Allegretto"
            , "Minuet. Allegretto"
            , "Allegro"
            ]
        , c.quartet
            "String_Quartet_No.5,_Op.18_No.5"
            [ "Allegro"
            , "Minuet - Trio"
            , "Andante cantabile con variazioni"
            , "Allegro"
            ]
        , c.quartet
            "String_Quartet_No.6,_Op.18_No.6"
            [ "Allegro con brio"
            , "Adagio, ma non troppo"
            , "La Malinconia. Adagio"
            , "Allegretto quasi Allegro"
            ]
        , c.quartet
            "String_Quartet_No.7,_Op.59_No.1"
            [ "Allegro"
            , "Allegretto vivace e sempre scherzando"
            , "Adagio molto e mesto"
            , "Thème russe. Allegro"
            ]
        , c.quartet
            "String_Quartet_No.8,_Op.59_No.2"
            [ "Allegro"
            , "Molto Adagio. Si tratta questo pezzo con molto di sentimento"
            , "Allegretto - Maggiore, Thème russe."
            , "Finale. Presto"
            ]
        , c.quartet
            "String_Quartet_No.9,_Op.59_No.3"
            [ "Introduzione. Andante con moto - Allegro vivace"
            , "Andante con moto quasi Allegretto"
            , "Minuet. Grazioso - Trio"
            , "Allegro molto"
            ]
        , c.quartet
            "String_Quartet_No.10,_Op.74"
            [ "Poco adagio - Allegro"
            , "Adagio ma non troppo"
            , "Presto - Più presto quasi prestissimo"
            , "Allegretto con variazioni"
            ]
        , c.quartet
            "String_Quartet_No.11,_Op.95"
            [ "Allegro con brio"
            , "Allegretto ma non troppo"
            , "Allegro assai vivace ma serioso - Trio"
            , "Larghetto espressivo - Allegretto agitato"
            ]
        , c.quartet
            "String_Quartet_No.12,_Op.127"
            [ "Maestoso - Allegro"
            , "Adagio, ma non troppo e molto cantabile"
            , "Scherzando vivace - Trio"
            , "Finale. Allegro"
            ]
        , c.quartet
            "String_Quartet_No.13,_Op.130"
            [ "Adagio ma non troppo - Allegro"
            , "Presto"
            , "Andante con moto ma non troppo"
            , "Alla danza tedesca. Allegro assai"
            , "Cavatina. Adagio molto espressivo"
            , "Finale. Allegro"
            ]
        , c.quartet "Große_Fuge,_Op.133" [ "Allegro" ]
        , c.quartet
            "String_Quartet_No.14,_Op.131"
            [ "Adagio ma non troppo e molto espressivo"
            , "Allegro molto vivace"
            , "Allegro moderato (recitative)"
            , "Andante ma non troppo e molto cantabile"
            , "Presto"
            , "Adagio quasi un poco andante"
            , "Allegro"
            ]
        , c.quartet
            "String_Quartet_No.15,_Op.132"
            [ "Assai sostenuto - Allegro"
            , "Allegro ma non tanto"
            , "Molto adagio"
            , "Alla marcia, assai vivace"
            , "Allegro appassionato"
            ]
        , c.quartet
            "String_Quartet_No.16,_Op.135"
            [ "Allegretto"
            , "Vivace"
            , "Lento assai, cantante e tranquillo"
            , "Grave, ma non troppo tratto - Allegro"
            ]
        ]
      }
