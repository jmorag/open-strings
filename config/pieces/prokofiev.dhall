let Types = ./types.dhall

let c = ./composer.dhall "Prokofiev,_Sergey"

let composer_works =
        c
      ⫽ { works =
          [ c.concerto
              Types.violin
              "Violin_Concerto_No.1,_Op.19"
              [ "Andantino"
              , "Scerzo. Vivacissimo"
              , "Moderato. Allegro moderato"
              ]
          , c.concerto
              Types.violin
              "Violin_Concerto_No.2,_Op.63"
              [ "Allegro moderato", "Andante assai", "Allegro ben marcato" ]
          ]
        }

in  composer_works.(Types.Composer)
