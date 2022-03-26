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
    #:items (list (make-item 'gold #:amount 2))
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
    #:items '(anthead-monograph))

   (make-place
    #:id 'control-room
    #:features '(martaanvuo-console))

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


(define (make-new-world)

  (define places (make-places))

  (define routes
    (list
     #; (make-path-between perimeter martaanvuo-swamp 'hidden)
     (make-path-between places 'perimeter 'magpie-hill 110 #:no-encounters? #t)
     (make-path-between places 'perimeter 'martaanvuo-swamp 90 #:no-encounters? #t)
     (make-path-between places 'martaanvuo-swamp 'crematory 70)
     (make-path-between places 'martaanvuo-swamp 'martaanvuo-docks 60 #:no-encounters? #t)
     (make-path-between places 'martaanvuo-docks 'murkwater-docks 230 #:no-encounters? #t) ; temporary: this should require water transport!
     (make-path-between places 'martaanvuo-docks 'palsat 240 #:no-encounters? #t)
     (make-path-between places 'martaanvuo-swamp 'luminous-precipice 120)
     (make-path-between places 'crematory 'the-maw 1 #:no-encounters? #t)
     (make-path-between places 'the-maw 'waiting-room 1 #:no-encounters? #t #:onedirectional? #t)
     (make-path-between places 'magpie-hill 'power-plant-ruins 30 #:no-encounters? #t)
     (make-path-between places 'magpie-hill 'luminous-precipice 60 #:no-encounters? #t)
     (make-path-between places 'power-plant-ruins 'cache 2 #:no-encounters? #t #:details '(locked))
     (make-path-between places 'power-plant-ruins 'sewers-1 5)
     (make-path-between places 'sewers-1 'sewers-2 10)
     (make-path-between places 'sewers-1 'workshop 10)
     (make-path-between places 'sewers-1 'compound-entrance 10)
     (make-path-between places 'compound-entrance 'murkwater-docks 2)
     (make-path-between places 'compound-entrance 'workshop 2)
     (make-path-between places 'murkwater-docks 'workshop 2 #:no-encounters? #t)
     (make-path-between places 'murkwater-docks 'palsat 130 #:no-encounters? #t)
     (make-path-between places 'sewers-2 'storage-closet 1)
     (make-path-between places 'storage-closet 'workshop 1)
     (make-path-between places 'workshop 'control-room 2 #:no-encounters? #t)
     (make-path-between places 'workshop 'martaanvuo-source 2)
     (make-path-between places 'control-room 'reactor-room 1)
     ))

  (world places routes 0 0))


; not content

; Uniqueness constraints(?), unidirectional paths(?), yada yada
; NB: Modifies a and b in places, and returns route r between the two
(define (make-path-between
         places
         id-a
         id-b
         traverse-time
         #:hidden? [hidden? #f]
         #:no-encounters? [no-encounters? #f]
         #:onedirectional? [onedirectional? #f]
         #:details [details '()])

  (define (find-place place-id)
    (findf (λ (place) (eq? (location-id place) place-id))
           places))

  (define place-a (find-place id-a))
  (define place-b (find-place id-b))

  (when no-encounters? (set! details (append-element details 'no-encounters)))

  (define actors '())
  (define r (make-route
             id-a
             id-b
             traverse-time
             #:details details
             #:actors actors))
  (define route-id (location-id r))
  (set-place-routes! place-a (append-element (place-routes place-a) route-id))
  (when (not onedirectional?)
    (set-place-routes! place-b (append-element (place-routes place-b) route-id)))
  (when hidden? (error "Implement hidden paths"))
  r)

(provide get-route-by-id)
(define (get-route-by-id id)
  (define w (current-world))
  (define routes (world-routes w))
  (define r (findf (λ (route) (eq? id (location-id route)))
                   routes))
  (if r r '()))

(provide get-place-by-id)
(define (get-place-by-id id)
  (define w (current-world))
  (define places (world-places w))
  (define r (findf (λ (place) (eq? (location-id place) id))
                   places))
  (if r r '()))

(provide get-location-by-id)
(define (get-location-by-id id)
  (cond ((not (null? (get-place-by-id id)))
         (get-place-by-id id))
        ((not (null? (get-route-by-id id)))
         (get-route-by-id id))
        (else '())))
