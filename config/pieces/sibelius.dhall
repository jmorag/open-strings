let Types = ./types.dhall

let c = ./composer.dhall "Sibelius,_Jean"

let composer_works =
        c
      ⫽ { works =
          [ c.concerto
              Types.violin
              "Violin_Concerto,_Op.47"
              [ "Allegro moderato", "Adagio di molto", "Allegro, ma non tanto" ]
          ]
        }

in  composer_works.(Types.Composer)
