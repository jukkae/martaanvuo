#lang at-exp racket

(provide (all-defined-out))

(require
  "../../0-engine/4-systems/world/world.rkt"

  "../../0-engine/2-core/core.rkt"

  "../../0-engine/3-types/location.rkt"
  "../../0-engine/3-types/route.rkt"
  "../../0-engine/3-types/world.rkt"

  "../../0-engine/4-systems/actors/actor.rkt"
  "../../0-engine/4-systems/items/item.rkt"

  "../../0-engine/7-state/state.rkt"
  )


;  Think in terms of acquisition and attrition: First phase, gather equipment and tools; second phase: live it down
;  Negative sum game: Every possible outcome is worse than how it was before; "the only winning move is to not play"
;  - a sort of an event horizon!

(define (make-places)
  (list

   (make-place
    #:id 'perimeter
    #:type 'perimeter
    #:shortname "Perimeter")

   (make-place
    #:id 'martaanvuo-swamp
    #:type 'swamp
    #:features '(anthill)
    #:shortname "Martaanvuo swamp")

   (make-place
    #:id 'magpie-hill
    #:features '(magpie-effigy)
    #:type 'mountains
    #:shortname "Magpie hill")

   (make-place
    #:id 'luminous-precipice
    #:features '(precipice)
    #:type 'mountains
    #:shortname "Precipice")

   (make-place
    #:id 'burnt-tree
    #:type 'burnt-tree
    #:shortname "Burnt tree")

   (make-place
    #:id 'martaanvuo-docks
    #:features '(mieli)
    #:type 'docks
    #:shortname "Martaanvuo docks")

   (make-place
    #:id 'outpost
    #:type 'outpost
    #:shortname "Scientific outpost")

   (make-place
    #:id 'tunnels-1
    #:type 'tunnels
    #:items (list (make-item 'ammo)))

   (make-place
    #:id 'tunnels-2
    #:type 'tunnels
    #:items (list (make-item 'ammo)))

   (make-place
    #:id 'cache
    #:items (list (make-item 'gold #:amount 7))
    #:type 'cache)

   (make-place
    #:id 'workshop
    #:features '(hartmann-device)
    #:type 'workshop)

   (make-place
    #:id 'compound-entrance)

   (make-place
    #:id 'murkwater-docks
    #:type 'docks)

   (make-place
    #:id 'storage-closet
    #:features '(martaanvuo-book))

   (make-place
    #:id 'control-room
    #:features '(martaanvuo-terminal))

   (make-place
    #:id 'reactor-room)

   (make-place
    #:id 'martaanvuo-source)

   (make-place
    #:id 'the-maw)

   (make-place
    #:id 'slaughterhouse)

   (make-place
    #:id 'waiting-room
    #:features '(waiting-room-begin))

   (make-place
    #:id 'palsat)))

(provide make-new-world)
(define (make-new-world)

  (define places (make-places))

  (define routes
    (list
     #; (make-path-between perimeter martaanvuo-swamp 'hidden)
     (make-path-between places 'perimeter 'magpie-hill 110 #:no-encounters? #t)
     (make-path-between places 'perimeter 'martaanvuo-swamp 90 #:no-encounters? #t)
     (make-path-between places 'martaanvuo-swamp 'burnt-tree 70)
     (make-path-between places 'martaanvuo-swamp 'martaanvuo-docks 60 #:no-encounters? #t)
    ;  (make-path-between places 'martaanvuo-docks 'murkwater-docks 230 #:no-encounters? #t) ; temporary: this should require water transport!
    ;  (make-path-between places 'martaanvuo-docks 'palsat 240 #:no-encounters? #t)
    ;  (make-path-between places 'martaanvuo-swamp 'luminous-precipice 120)
     (make-path-between places 'burnt-tree 'the-maw 1 #:no-encounters? #t)
     (make-path-between places 'the-maw 'waiting-room 1 #:no-encounters? #t #:onedirectional? #t)
     (make-path-between places 'magpie-hill 'outpost 30 #:no-encounters? #t)
     (make-path-between places 'magpie-hill 'luminous-precipice 60 #:no-encounters? #t)
     (make-path-between places 'outpost 'cache 2 #:no-encounters? #t #:details '(locked))
     (make-path-between places 'outpost 'tunnels-1 5 #:no-encounters? #t)
     (make-path-between places 'tunnels-1 'tunnels-2 10)
     (make-path-between places 'tunnels-1 'workshop 10)
     (make-path-between places 'tunnels-1 'compound-entrance 10)
     (make-path-between places 'compound-entrance 'murkwater-docks 2)
     (make-path-between places 'compound-entrance 'workshop 2)
     (make-path-between places 'murkwater-docks 'workshop 2 #:no-encounters? #t)
     (make-path-between places 'murkwater-docks 'palsat 130 #:no-encounters? #t)
     (make-path-between places 'tunnels-2 'storage-closet 1)
     (make-path-between places 'storage-closet 'workshop 1 #:no-encounters? #t)
     (make-path-between places 'workshop 'control-room 2 #:no-encounters? #t)
     (make-path-between places 'workshop 'martaanvuo-source 2)
     (make-path-between places 'control-room 'reactor-room 1)
     ))

  (world places routes 0 0))

