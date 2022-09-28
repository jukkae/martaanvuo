#lang at-exp racket

(provide (all-defined-out))

(require
  "../../0-engine/0-api/api.rkt"
  )

(define places
  (list
   (place
    #:id 'perimeter
    #:type 'ext
    #:encounter-types '(voidfloater)
    #:shortname "Perimeter")

   (place
    #:id 'martaanvuo-swamp
    #:type 'ext
    #:features '(anthill)
    #:shortname "Martaanvuo swamp")

   (place
    #:id 'magpie-hill
    #:features '(magpie-effigy)
    #:items (list (make-item 'revolver))
    #:type 'ext
    #:shortname "Magpie hill")

   (place
    #:id 'luminous-precipice
    #:features '(precipice)
    #:type 'ext
    #:shortname "Precipice")

   (place
    #:id 'burnt-tree
    #:type 'ext
    #:shortname "Burnt tree")

   (place
    #:id 'martaanvuo-docks
    #:choices
    (list
     (make-choice
      'buy-mods
      "Visit the Forge of Master Seppo"
      `(
        (go-to-fragment 'seppo)
        '()
        ))
    )
    #:type 'ext
    #:shortname "Martaanvuo docks")

   (place
    #:id 'outpost
    #:type 'int
    #:shortname "Scientific outpost")

   (place
    #:id 'tunnels-1
    #:type 'int
    #:items (list (make-item 'ammo)))

   (place
    #:id 'tunnels-2
    #:type 'int
    #:items (list (make-item 'ammo)))

   (place
    #:id 'cache
    #:items (list (make-item 'gold #:amount 7))
    #:type 'int)

   (place
    #:id 'workshop
    #:shortname "Workshop"
    #:features '(martaanvuo-terminal)
    #:type 'int)

   (place
    #:id 'compound-entrance
    #:type 'int)

   (place
    #:id 'murkwater-docks
    #:type 'ext
    )

   (place
    #:id 'storage-closet
    #:type 'int
    #:features '(martaanvuo-book))

   (place
    #:id 'control-room
    #:type 'int
    #:features '(hartmann-device)
    )

   (place
    #:id 'reactor-room
    #:type 'int
    #:features '(teleporter))

   (place
    #:id 'martaanvuo-source
    #:type 'ext)

   (place
    #:id 'the-maw
    #:type 'int
    #:size 'small
    #:tags '(cluttered)
    #:shortname "The Maw"
    #:encounter-types '(limbtearer voidfloater blindscraper two-blindscrapers)
    #:light-level 'dark
    #:features (list 'the-button 'light-switch 'running-centrifuge)
    )

   (place
    #:id 'slaughterhouse
    #:type 'int)

   (place
    #:id 'waiting-room
    #:type 'int
    #:features '(waiting-room-begin))

   (place
    #:id 'palsat
    #:type 'ext)

   (place
    #:id 'carnival
    #:type 'ext
    #:features '(the-endless-staircase bobo-the-clown the-merchant fortune-teller)) ; not the same merchant, not literally at least
  ))

(define routes
  (list
    ; (route-between perimeter martaanvuo-swamp 'hidden)
    (route-between 'perimeter 'magpie-hill 110 'ext)
    (route-between 'perimeter 'martaanvuo-docks 90 'ext #:encounter-types '(voidfloater))
    (route-between 'martaanvuo-swamp 'burnt-tree 70 'ext #:encounter-types '(voidfloater))
    #;(route-between 'martaanvuo-swamp 'martaanvuo-docks 60 'ext #:no-encounters? #t)
    #;(route-between 'martaanvuo-docks 'carnival 250 'ext #:no-encounters? #t) ; water transport
    #;(route-between 'martaanvuo-docks 'forge 5 'ext #:no-encounters? #t)
    ; (route-between 'martaanvuo-docks 'the-ring-of-surut 5 #:no-encounters? #t)
    ; (route-between places 'martaanvuo-docks 'murkwater-docks 230 #:no-encounters? #t) ; temporary: this should require water transport!
    ; (route-between places 'martaanvuo-docks 'palsat 240 #:no-encounters? #t)
    ; (route-between places 'martaanvuo-swamp 'luminous-precipice 120)
    (route-between 'burnt-tree 'the-maw 1 'int #:no-encounters? #t)
    (route-between 'martaanvuo-docks 'the-maw 5 'int #:light-level 'dark)
    ; (route-between 'the-maw 'waiting-room 1 'int #:no-encounters? #t #:one-directional? #t)
    (route-between 'magpie-hill 'outpost 30 'ext #:no-encounters? #t)
    #;(route-between 'magpie-hill 'luminous-precipice 60 'ext #:no-encounters? #t)
    (route-between 'outpost 'cache 2 'int #:no-encounters? #t #:details '(locked))
    (route-between 'outpost 'workshop 2 'int #:no-encounters? #t)
    #;(route-between 'outpost 'tunnels-1 5 'int #:no-encounters? #t)
    (route-between 'tunnels-1 'tunnels-2 10 'int)
    #;(route-between 'tunnels-1 'workshop 10 'int)
    (route-between 'tunnels-1 'compound-entrance 10 'int)
    (route-between 'compound-entrance 'murkwater-docks 2 'int)
    #;(route-between 'compound-entrance 'workshop 2 'int)
    #;(route-between 'murkwater-docks 'workshop 2 'ext #:no-encounters? #t)
    (route-between 'murkwater-docks 'palsat 130 'ext #:no-encounters? #t)
    (route-between 'tunnels-2 'storage-closet 1 'ext)
    #;(route-between 'storage-closet 'workshop 1 'int #:no-encounters? #t)
    #;(route-between 'workshop 'control-room 2 'int #:no-encounters? #t)
    #;(route-between 'workshop 'martaanvuo-source 2 'int)
    (route-between 'control-room 'reactor-room 1 'int)
  ))

(provide make-new-world)
(define (make-new-world)
  (connect-places-and-routes! places routes)
  (world places routes 0 0))
