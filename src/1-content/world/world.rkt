#lang at-exp racket

(provide (all-defined-out))

(require
  "../../0-engine/0-api/api.rkt"
  )

(define places
  (list
   (place
    #:id 'perimeter
    #:type 'perimeter
    #:encounter-types '(voidfloater)
    #:shortname "Perimeter")

   (place
    #:id 'martaanvuo-swamp
    #:type 'swamp
    #:features '(anthill)
    #:shortname "Martaanvuo swamp")

   (place
    #:id 'magpie-hill
    #:features '(magpie-effigy)
    #:items (list (make-item 'revolver))
    #:type 'mountains
    #:shortname "Magpie hill")

   (place
    #:id 'luminous-precipice
    #:features '(precipice)
    #:type 'mountains
    #:shortname "Precipice")

   (place
    #:id 'burnt-tree
    #:type 'burnt-tree
    #:shortname "Burnt tree")

   (place
    #:id 'martaanvuo-docks
    #:features '(mieli)
    #:type 'docks
    #:shortname "Martaanvuo docks")

   (place
    #:id 'outpost
    #:type 'outpost
    #:shortname "Scientific outpost")

   (place
    #:id 'tunnels-1
    #:type 'tunnels
    #:items (list (make-item 'ammo)))

   (place
    #:id 'tunnels-2
    #:type 'tunnels
    #:items (list (make-item 'ammo)))

   (place
    #:id 'cache
    #:items (list (make-item 'gold #:amount 7))
    #:type 'cache)

   (place
    #:id 'workshop
    #:features '(hartmann-device)
    #:type 'workshop)

   (place
    #:id 'compound-entrance)

   (place
    #:id 'murkwater-docks
    #:type 'docks)

   (place
    #:id 'storage-closet
    #:features '(martaanvuo-book))

   (place
    #:id 'control-room
    #:features '(martaanvuo-terminal))

   (place
    #:id 'reactor-room
    #:features '(teleporter))

   (place
    #:id 'martaanvuo-source)

   (place
    #:id 'the-maw)

   (place
    #:id 'slaughterhouse)

   (place
    #:id 'waiting-room
    #:features '(waiting-room-begin))

   (place
    #:id 'palsat)

   (place
    #:id 'carnival
    #:features '(the-endless-staircase bobo-the-clown the-merchant fortune-teller)) ; not the same merchant, not literally at least

    (place
    #:id 'ladder-of-surut
    #:type 'indoors
    #:shortname "Ladder of Surut"
    #:encounter-types '(voidfloater)
    #:choices
    (list
     (choice
          'ring-new-enemy-bell
          "Ring the Bell of Surut"
          `(
            (spawn-encounter)
            '()
            ))
      )

      )
  ))

(define routes
  (list
    ; (route-between perimeter martaanvuo-swamp 'hidden)
    (route-between 'perimeter 'magpie-hill 110)
    (route-between 'perimeter 'martaanvuo-docks 90 #:encounter-types '(human-fighter))
    (route-between 'martaanvuo-swamp 'burnt-tree 70 #:encounter-types '(human-fighter))
    #;(route-between 'martaanvuo-swamp 'martaanvuo-docks 60 #:no-encounters? #t)
    #;(route-between 'martaanvuo-docks 'carnival 250 #:no-encounters? #t) ; water transport
    #;(route-between 'martaanvuo-docks 'forge 5 #:no-encounters? #t)
    ; (route-between 'martaanvuo-docks 'the-ring-of-surut 5 #:no-encounters? #t)
    ; (route-between places 'martaanvuo-docks 'murkwater-docks 230 #:no-encounters? #t) ; temporary: this should require water transport!
    ; (route-between places 'martaanvuo-docks 'palsat 240 #:no-encounters? #t)
    ; (route-between places 'martaanvuo-swamp 'luminous-precipice 120)
    (route-between 'burnt-tree 'the-maw 1 #:no-encounters? #t)
    (route-between 'the-maw 'waiting-room 1 #:no-encounters? #t #:one-directional? #t)
    #;(route-between 'magpie-hill 'outpost 30 #:no-encounters? #t)
    #;(route-between 'magpie-hill 'luminous-precipice 60 #:no-encounters? #t)
    (route-between 'outpost 'cache 2 #:no-encounters? #t #:details '(locked))
    (route-between 'outpost 'tunnels-1 5 #:no-encounters? #t)
    (route-between 'tunnels-1 'tunnels-2 10)
    (route-between 'tunnels-1 'workshop 10)
    (route-between 'tunnels-1 'compound-entrance 10)
    (route-between 'compound-entrance 'murkwater-docks 2)
    (route-between 'compound-entrance 'workshop 2)
    (route-between 'murkwater-docks 'workshop 2 #:no-encounters? #t)
    (route-between 'murkwater-docks 'palsat 130 #:no-encounters? #t)
    (route-between 'tunnels-2 'storage-closet 1)
    (route-between 'storage-closet 'workshop 1 #:no-encounters? #t)
    (route-between 'workshop 'control-room 2 #:no-encounters? #t)
    (route-between 'workshop 'martaanvuo-source 2)
    (route-between 'control-room 'reactor-room 1)
  ))

(provide make-new-world)
(define (make-new-world)
  (connect-places-and-routes! places routes)
  (world places routes 0 0))
