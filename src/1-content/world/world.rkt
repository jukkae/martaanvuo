#lang at-exp racket

(provide (all-defined-out))

(require
  ; ; TODO: Globber!
  ; "locations/ladder-of-surut.rkt"

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
    #:id 'reactor-room
    #:features '(teleporter))

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
    #:id 'palsat)

   (make-place
    #:id 'carnival
    #:features '(the-endless-staircase bobo-the-clown the-merchant fortune-teller)) ; not the same merchant, not literally at least

    (make-place
    #:id 'ladder-of-surut
    #:features '(new-enemy-bell)
    #:type 'indoors
    #:shortname "Ladder of Surut"
    #:encounter-types '(voidfloater))
  ))

(define (make-routes)
(list
     #; (make-route-between perimeter martaanvuo-swamp 'hidden)
     (make-route-between 'perimeter 'magpie-hill 110 #:no-encounters? #t)
     (make-route-between 'perimeter 'martaanvuo-swamp 90 #:no-encounters? #t)
     (make-route-between 'martaanvuo-swamp 'burnt-tree 70 #:encounter-types '(human-fighter))
     (make-route-between 'martaanvuo-swamp 'martaanvuo-docks 60 #:no-encounters? #t)
     (make-route-between 'martaanvuo-docks 'carnival 250 #:no-encounters? #t) ; water transport
     (make-route-between 'martaanvuo-docks 'ladder-of-surut 1 #:no-encounters? #t)
    ;  (make-route-between places 'martaanvuo-docks 'murkwater-docks 230 #:no-encounters? #t) ; temporary: this should require water transport!
    ;  (make-route-between places 'martaanvuo-docks 'palsat 240 #:no-encounters? #t)
    ;  (make-route-between places 'martaanvuo-swamp 'luminous-precipice 120)
     (make-route-between 'burnt-tree 'the-maw 1 #:no-encounters? #t)
     (make-route-between 'the-maw 'waiting-room 1 #:no-encounters? #t #:one-directional? #t)
     (make-route-between 'magpie-hill 'outpost 30 #:no-encounters? #t)
     (make-route-between 'magpie-hill 'luminous-precipice 60 #:no-encounters? #t)
     (make-route-between 'outpost 'cache 2 #:no-encounters? #t #:details '(locked))
     (make-route-between 'outpost 'tunnels-1 5 #:no-encounters? #t)
     (make-route-between 'tunnels-1 'tunnels-2 10)
     (make-route-between 'tunnels-1 'workshop 10)
     (make-route-between 'tunnels-1 'compound-entrance 10)
     (make-route-between 'compound-entrance 'murkwater-docks 2)
     (make-route-between 'compound-entrance 'workshop 2)
     (make-route-between 'murkwater-docks 'workshop 2 #:no-encounters? #t)
     (make-route-between 'murkwater-docks 'palsat 130 #:no-encounters? #t)
     (make-route-between 'tunnels-2 'storage-closet 1)
     (make-route-between 'storage-closet 'workshop 1 #:no-encounters? #t)
     (make-route-between 'workshop 'control-room 2 #:no-encounters? #t)
     (make-route-between 'workshop 'martaanvuo-source 2)
     (make-route-between 'control-room 'reactor-room 1)
     )
)

(define (connect-places-and-routes! places routes)
  (define (find-place place-id)
    (findf (λ (place) (eq? (location-id place) place-id))
           places))
  (for ([route routes])
    (define route-id (location-id route))
    (define id-a (route-a route))
    (define id-b (route-b route))
    (define place-a (find-place id-a))
    (define place-b (find-place id-b))
    (set-place-routes! place-a (append-element (place-routes place-a) route-id))
    (when (not (route-one-directional? route))
      (set-place-routes! place-b (append-element (place-routes place-b) route-id)))
    )
)

(provide make-new-world)
(define (make-new-world)
  (define places (make-places))
  (define routes (make-routes))
  (connect-places-and-routes! places routes)
  (world places routes 0 0))

