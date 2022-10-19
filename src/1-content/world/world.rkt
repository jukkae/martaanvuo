#lang at-exp racket

(provide (all-defined-out))

(require
  "../../0-engine/0-api/api.rkt"
  )

; See scenario.rkt for map overview
(define places
  (list
   (place
    #:id 'perimeter
    #:size 'large
    #:type 'ext
    #:encounter-types '(voidfloater)
    #:hidden-features (list 'cocoon-effigy)
    #:zones
    (list
      (zone ; remains of an adventurer
       #:clue
       (clue
        #:requires (SenseOrgan 'nose 2 "")
        #:description "smell of decaying flesh"
        )
       #:interactibles (list (make-item 'knife) (make-item 'lighter))
       #:name "The body of a dead adventurer"
       #:description "at the body of a dead adventurer"
       )
      (zone
       #:clue
       (clue
        #:requires (SenseOrgan 'eyes 1 "")
        #:description "old vehicle tracks"
        )
       #:interactibles (list (make-item 'can-of-gas))
       #:name "Abandoned car"
       #:description "at the abandoned car"
       )
      (zone
       #:clue
       (clue
        #:requires (SenseOrgan 'nose 2 "")
        #:description "faint noxious smell of a thiefbird nest"
        )
       #:interactibles (list (make-item 'revolver) (make-item 'gold #:amount 3))
       #:name "thiefbird nest"
       #:description "at a thiefbird nest"
       )
      (zone
       #:clue
       (clue
        #:requires (SenseOrgan 'ears 1 "")
        #:description "sound of running water"
        )
       #:interactibles '(brook)
       #:name "a brook"
       #:description "at a brook"
       )
      )
    #:clues
    '()
    #:shortname "perimeter")

   (place
    #:id 'magpie-hill
    #:size 'large
    #:type 'ext
    #:shortname "Magpie hill")

  (place
    #:id 'lookout
    #:features '()
    #:type 'int)

   (place
    #:id 'shack
    #:features '()
    #:items (list (make-item 'shovel) (make-item 'shotgun))
    #:type 'int)

   (place
    #:id 'pond-of-drowning
    #:features (list 'caterpillar-effigy)
    #:items (list (make-item 'gold #:amount 7) (make-item 'human-remains))
    #:type 'ext
    #:shortname "pond")


   (place
    #:id 'martaanvuo-dam
    #:features '()
    #:type 'ext
    #:shortname "Martaanvuo dam")

   (place
    #:id 'martaanvuo-river
    #:features '()
    #:type 'ext
    #:shortname "Martaanvuo river")

   (place #:id 'abandoned-village
    #:features (list 'baseball-bat 'fuel-can)
    #:type 'ext
    #:shortname "Abandoned village")

   (place #:id 'tunnels-1
    #:features '()
    #:items (list (make-item 'shaman-bag))
    #:type 'int
    #:light-level 'pitch-black
    #:shortname "Mining tunnels")

   (place #:id 'tunnels-2
    #:features '()
    #:type 'int
    #:light-level 'pitch-black
    #:shortname "Mining tunnels")

   (place #:id 'tunnels-3
    #:features '()
    #:type 'int
    #:light-level 'pitch-black
    #:shortname "Mining tunnels")

   (place #:id 'tunnels-4
    #:features '()
    #:type 'int
    #:light-level 'pitch-black
    #:shortname "Mining tunnels")

   (place #:id 'gas-station
    #:features (list 'hostile-gang)
    #:type 'ext
    #:shortname "Gas station")

   (place #:id 'highway-1
    #:type 'ext
    #:shortname "Highway")

   (place #:id 'highway-2
    #:type 'ext
    #:shortname "Highway")

   (place #:id 'highway-3
    #:type 'ext
    #:shortname "Highway")


   (place
    #:id 'village
    #:choices
    (list
     (make-choice
      'visit-forge
      "Visit the village forge"
      `(
        (go-to-fragment 'seppo)
        '()
        ))
     (make-choice
      'visit-shaman
      "Visit the village shaman"
      `(
        (go-to-fragment 'shaman)
        '()
        ))
    )
    #:type 'ext
    #:shortname "Village")


   (place
    #:id 'the-maw
    #:type 'int
    #:size 'small
    #:tags '(cluttered)
    #:shortname "The Maw"
    #:encounter-types '(limbtearer voidfloater blindscraper two-blindscrapers)
    #:light-level 'dark
    #:features (list 'light-switch 'martaanvuo-terminal)
    )

  (place
    #:id 'reactor-room
    #:type 'int
    #:size 'small
    #:tags '(cluttered)
    #:shortname "reactor room"
    #:encounter-types '(limbtearer voidfloater blindscraper two-blindscrapers)
    #:light-level 'dark
    #:features (list 'the-button 'running-centrifuge)
    )

  (place
    #:id 'bioreactor
    #:type 'int
    #:size 'container
    #:tags '()
    #:shortname "bioreactor"
    #:encounter-types '(limbtearer voidfloater blindscraper two-blindscrapers)
    #:light-level 'dark
    #:features (list 'the-button)
    )
  ))

(define routes
  (list
    (route-between 'perimeter 'magpie-hill
      130
      'ext
      #:hidden? #f
      #:descr-from-a "steep, rocky ascent"
      #:descr-from-b "steep, rocky descent"
      #:encounter-types '(voidfloater)
      )
    (route-between 'magpie-hill 'lookout 10 'ext #:hidden? #f #:descr-from-a "path to the ledge" #:descr-from-b "path back to the plateau")
    (route-between 'magpie-hill 'shack 40 'ext #:hidden? #t #:descr-from-a "rocky path" #:descr-from-b "rocky path")
    (route-between 'magpie-hill 'pond-of-drowning 30 'ext #:hidden? #t #:descr-from-a "footsteps through trampled underbrush" #:descr-from-b "path through prickly underbrush")

    (route-between 'perimeter 'martaanvuo-dam 140 'ext #:hidden? #t #:descr-from-a "winding path downhill" #:descr-from-b "winding path uphill")
    (route-between 'martaanvuo-dam 'martaanvuo-river 40 'ext #:descr-from-a "crumbling stairs downriver" #:descr-from-b "crumbling stairs back to the dam")
    (route-between 'martaanvuo-river 'abandoned-village 130 'ext #:descr-from-a "downriver the dry riverbank" #:descr-from-b "upriver the dry riverbank")

    (route-between 'abandoned-village 'village 60 'ext #:descr-from-a "downriver the dry riverbank" #:descr-from-b "upriver the dry riverbank")
    (route-between 'abandoned-village 'gas-station 5 'ext #:hidden? #t #:descr-from-a "beaten tarmac track" #:descr-from-b "beaten tarmac track")

    (route-between 'gas-station 'highway-1  45 'ext #:descr-from-a "highway" #:descr-from-b "tarmac")
    (route-between 'highway-1 'highway-2 45 'ext #:descr-from-a "highway (west)" #:descr-from-b "highway (east)")
    (route-between 'highway-2 'highway-2 45 #:one-directional? #t 'ext #:descr-from-a "highway (west)" #:descr-from-b "highway (east)")
    (route-between 'highway-1 'highway-3 45 'ext #:descr-from-a "highway (east)" #:descr-from-b "highway (west)")
    (route-between 'highway-3 'highway-3 45 #:one-directional? #t 'ext #:descr-from-a "highway (east)" #:descr-from-b "highway (west)")

    (route-between 'abandoned-village 'tunnels-1 25 'int #:hidden? #t #:descr-from-a "rundown mining shaft ladders" #:descr-from-b "rundown mining shaft ladders")

    (route-between 'tunnels-1 'tunnels-2 26 'int #:descr-from-a "tunnels" #:descr-from-b "tunnels")
    (route-between 'tunnels-2 'tunnels-3 27 'int #:descr-from-a "tunnels" #:descr-from-b "tunnels")
    (route-between 'tunnels-3 'tunnels-4 24 'int #:descr-from-a "tunnels" #:descr-from-b "tunnels")
    (route-between 'tunnels-3 'the-maw 25 'int #:hidden? #t #:descr-from-a "tunnels" #:descr-from-b "a hole to a tunnel")

    (route-between 'martaanvuo-dam 'the-maw 10 'ext #:hidden? #t #:descr-from-a "a hole in concrete wall behind corrugated iron" #:descr-from-b "a hole in concrete wall")
    (route-between 'the-maw 'reactor-room 1 'int #:descr-from-a "corridor" #:descr-from-b "corridor")
    (route-between 'reactor-room 'bioreactor 1 'int #:details (list 'only-when-small) #:descr-from-a "reactor door" #:descr-from-b "reactor door")
  ))

(provide make-new-world)
(define (make-new-world)
  (connect-places-and-routes! places routes)
  (world places routes 0 0))
