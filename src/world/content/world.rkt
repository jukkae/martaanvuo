#lang at-exp racket

(provide (all-defined-out))

(require "../0-types/world.rkt")

(require "../../actors/actor.rkt"
         "../../items/item.rkt"
         "../../locations/0-types/location.rkt"
         "../../core/utils.rkt")

(require racket/lazy-require)
(lazy-require ["../../state/state.rkt" (current-world)])

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
    #:id 'crematory
    #:type 'crematory
    #:features '(waiting-room-begin)
    #:shortname "Crematory")

   (make-place
    #:id 'martaanvuo-docks
    #:features '(mieli)
    #:type 'docks
    #:shortname "Martaanvuo docks")

   (make-place
    #:id 'power-plant-ruins
    #:type 'ruins
    #:shortname "Ruined power plant")

   (make-place
    #:id 'sewers-1
    #:type 'sewers
    #:items (list (make-item 'ammo)))

   (make-place
    #:id 'sewers-2
    #:type 'sewers
    #:items (list (make-item 'ammo)))

   (make-place
    #:id 'cache
    #:items '(gold)
    #:type 'cache)

   (make-place
    #:id 'workshop
    ;#:features '(hartmann-device)
    #:features '(martaanvuo-console)
    #:type 'workshop)

   (make-place
    #:id 'compound-entrance)

   (make-place
    #:id 'murkwater-docks
    #:type 'docks)

   (make-place
    #:id 'storage-closet
    #:items '(anthead-monograph))

   (make-place
    #:id 'control-room)

   (make-place
    #:id 'reactor-room)

   (make-place
    #:id 'martaanvuo-source)

   (make-place
    #:id 'the-maw)

   (make-place
    #:id 'slaughterhouse)

   (make-place
    #:id 'the-waiting-room)

   (make-place
    #:id 'palsat)))


(define (make-new-world)

  (define places (make-places))

  (define routes
    (list
    #; (make-path-between perimeter martaanvuo-swamp 'hidden)
    (make-path-between places 'perimeter 'magpie-hill #:no-encounters? #t)
    (make-path-between places 'perimeter 'martaanvuo-swamp #:no-encounters? #t)
    (make-path-between places 'martaanvuo-swamp 'crematory)
    (make-path-between places 'martaanvuo-swamp 'martaanvuo-docks #:no-encounters? #t)
    (make-path-between places 'martaanvuo-docks 'murkwater-docks #:no-encounters? #t) ; temporary: this should require water transport!
    (make-path-between places 'martaanvuo-docks 'palsat #:no-encounters? #t)
    (make-path-between places 'martaanvuo-swamp 'luminous-precipice)
    (make-path-between places 'magpie-hill 'power-plant-ruins #:no-encounters? #t)
    (make-path-between places 'magpie-hill 'luminous-precipice #:no-encounters? #t)
    (make-path-between places 'power-plant-ruins 'cache #:no-encounters? #t #:details '(locked))
    (make-path-between places 'power-plant-ruins 'sewers-1)
    (make-path-between places 'sewers-1 'sewers-2)
    (make-path-between places 'sewers-1 'workshop)
    (make-path-between places 'sewers-1 'compound-entrance)
    (make-path-between places 'compound-entrance 'murkwater-docks)
    (make-path-between places 'compound-entrance 'workshop)
    (make-path-between places 'murkwater-docks 'workshop #:no-encounters? #t)
    (make-path-between places 'murkwater-docks 'palsat #:no-encounters? #t)
    (make-path-between places 'sewers-2 'storage-closet)
    (make-path-between places 'storage-closet 'workshop)
    (make-path-between places 'workshop 'control-room #:no-encounters? #t)
    (make-path-between places 'workshop 'martaanvuo-source)
    (make-path-between places 'control-room 'reactor-room)
  ))

  (world places routes 0 0))


; not content

(define *number-of-routes* 0)
; Uniqueness constraints(?), unidirectional paths(?), yada yada
; NB: Modifies a and b, and returns route r between the two
(define (make-path-between
         places
         id-a
         id-b
         #:hidden? [hidden? #f]
         #:no-encounters? [no-encounters? #f]
         #:details [details '()])

  (define place-a (find-place-by-id places id-a))
  (define place-b (find-place-by-id places id-b))
  (set! *number-of-routes* (add1 *number-of-routes*))

  (when no-encounters? (set! details (append-element details 'no-encounters)))

  (define actors '())
  (define route-id *number-of-routes*)
  (define r (make-route
             route-id
             id-a
             id-b
             #:details details
             #:actors actors))
  (set-place-routes! place-a (append-element (place-routes place-a) route-id))
  (set-place-routes! place-b (append-element (place-routes place-b) route-id))
  (when hidden? (error "Implement hidden paths"))
  r)

(define (find-place-by-id places id)
  (findf (λ (place) (eq? (location-id place) id))
         places))
