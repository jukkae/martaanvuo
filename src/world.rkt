#lang racket

(provide (all-defined-out))

(require racket/serialize)

(require "actor.rkt")
(require "item.rkt")
(require "locations/location.rkt")
(require "locations/place.rkt")
(require "locations/route.rkt")
(require "utils.rkt")

(serializable-struct
 world
 (day
  [elapsed-time #:mutable]))

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
    #:id 'bright-precipice
    #:features '(precipice)
    #:type 'mountains
    #:shortname "Precipice")

   (make-place
    #:id 'crematory
    #:type 'crematory
    #:shortname "Crematory")

   (make-place
    #:id 'martaanvuo-docks
    #:features '(stiltman)
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
   ))

(define *places* '())

(define (find-place-by-id id)
  (findf (λ (place) (location-is? id place))
         *places*))

(define *number-of-routes* 0)
; Uniqueness constraints(?), unidirectional paths(?), yada yada
(define (make-path-between
         id-a
         id-b
         #:hidden? [hidden? #f]
         #:no-encounters? [no-encounters? #f]
         #:details [details '()])
  
  (define place-a (find-place-by-id id-a))
  (define place-b (find-place-by-id id-b))
  (set! *number-of-routes* (add1 *number-of-routes*))

  (when no-encounters? (set! details (append-element details 'no-encounters)))
  
  (define actors '())
  (define r (make-route
             *number-of-routes*
             place-a
             place-b
             #:details details
             #:actors actors))
  (set-place-routes! place-a (append-element (place-routes place-a) r))
  (set-place-routes! place-b (append-element (place-routes place-b) r))
  (when hidden? (error "Implement hidden paths")))

(define (setup-world)
  (set! *places* (make-places))
  
  #;(make-path-between perimeter martaanvuo-swamp 'hidden)
  (make-path-between 'perimeter 'magpie-hill #:no-encounters? #t)
  (make-path-between 'perimeter 'martaanvuo-swamp #:no-encounters? #t)
  (make-path-between 'martaanvuo-swamp 'crematory)
  (make-path-between 'martaanvuo-swamp 'martaanvuo-docks #:no-encounters? #t)
  (make-path-between 'martaanvuo-docks 'murkwater-docks #:no-encounters? #t) ; temporary: this should require water transport!
  (make-path-between 'martaanvuo-swamp 'bright-precipice)
  (make-path-between 'magpie-hill 'power-plant-ruins #:no-encounters? #t)
  (make-path-between 'magpie-hill 'bright-precipice #:no-encounters? #t)
  (make-path-between 'power-plant-ruins 'cache #:no-encounters? #t #:details '(locked))
  (make-path-between 'power-plant-ruins 'sewers-1)
  (make-path-between 'sewers-1 'sewers-2)
  (make-path-between 'sewers-1 'workshop)
  (make-path-between 'sewers-1 'compound-entrance)
  (make-path-between 'compound-entrance 'murkwater-docks)
  (make-path-between 'compound-entrance 'workshop)
  (make-path-between 'murkwater-docks 'workshop #:no-encounters? #t)
  (make-path-between 'sewers-2 'storage-closet)
  (make-path-between 'storage-closet 'workshop)
  (make-path-between 'workshop 'control-room #:no-encounters? #t)
  (make-path-between 'workshop 'martaanvuo-source)
  (make-path-between 'control-room 'reactor-room)
  )


; world-as-simulation / scripting API
(define (remove-actor-from-its-current-location! actor)
  (define current-location (actor-location actor))
  (when (not (eq? '() current-location))
    (remove-actor-from-location! current-location actor)))

; world-as-simulation / scripting API
(provide move-actor-to-location!)
(define (move-actor-to-location! actor location)
  (remove-actor-from-its-current-location! actor)
  (set-actor-location! actor location)
  (add-actor-to-location! location actor))