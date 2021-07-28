#lang racket

(provide (all-defined-out))

(require racket/serialize)

(require "actor.rkt")
(require "item.rkt")
(require "location.rkt")
(require "utils.rkt")

(serializable-struct
 world
 (day
  [elapsed-time #:mutable]))

;;; This seems as good a place as any, so:

;   - the world is unspecifiedly post-apocalyptic
;
;   - weaponry is mostly improvised
;   - firearms are rare and they are mostly blackpowder-based old-west era or similar
;   - ... because modern weaponry requires smokeless powder, and supply is basically nonexistent
;   - the only ones that have any capability for automatics is Murkwater Aix, a corporation behind
;     the laboratory and the Anomaly
;   - ditto regarding high explosives etc

;;; Also:

;  Think in terms of acquisition and attrition: First phase, gather equipment and tools; second phase: live it down
;  Negative sum game: Every possible outcome is worse than how it was before; "the only winning move is to not play"
;  - a sort of an event horizon!

(define perimeter
  (make-location
   #:id 'perimeter
   #:type 'perimeter))

(define martaanvuo-swamp
  (make-location
   #:id 'martaanvuo-swamp
   #:type 'swamp))

(define magpie-hill
  (make-location
   #:id 'magpie-hill
   #:features '(magpie-effigy)
   #:type 'mountains))

(define crematory
  (make-location
   #:type 'crematory))

(define martaanvuo-docks
  (make-location
   #:id 'martaanvuo-docks
   #:type 'docks))

(define power-plant-ruins
  (make-location
   #:id 'power-plant-ruins
   #:features '(locked-door)
   #:type 'ruins))

(define sewers-1
  (make-location
   #:type 'sewers
   #:items (list (make-item 'ammo))))

(define sewers-2
  (make-location
   #:type 'sewers
   #:items (list (make-item 'ammo))))

(define cache
  (make-location
   #:id 'cache
   #:items '(u-235)
   #:type 'cache))

(define workshop
  (make-location
   #:id 'workshop
   #:features '(hartmann-device)
   #:type 'workshop))

(define compound-entrance
  (make-location
   #:id 'compound-entrance))

(define murkwater-docks
  (make-location
   #:id 'murkwater-docks
   #:type 'docks))

(define storage-closet
  (make-location
   #:id 'storage-closet))

(define control-room
  (make-location
   #:id 'control-room))

(define reactor-room
  (make-location
   #:id 'reactor-room))

(define martaanvuo-source
  (make-location
   #:id 'martaanvuo-source))


; Uniqueness constraints(?), unidirectional paths(?), yada yada
(define (make-path-between location-a location-b [hidden? #f])
  (set-location-neighbors! location-a (append-element (location-neighbors location-a) location-b))
  (set-location-neighbors! location-b (append-element (location-neighbors location-b) location-a))
  (when hidden? (error "Implement hidden paths")))

(define (setup-world)
  #;(make-path-between perimeter martaanvuo-swamp 'hidden)
  (make-path-between perimeter magpie-hill)
  (make-path-between perimeter martaanvuo-swamp)
  (make-path-between martaanvuo-swamp crematory)
  (make-path-between martaanvuo-swamp martaanvuo-docks)
  (make-path-between martaanvuo-swamp magpie-hill)
  (make-path-between magpie-hill power-plant-ruins)
  (make-path-between power-plant-ruins cache)
  (make-path-between power-plant-ruins sewers-1)
  (make-path-between sewers-1 sewers-2)
  (make-path-between sewers-1 workshop)
  (make-path-between sewers-1 compound-entrance)
  (make-path-between compound-entrance murkwater-docks)
  (make-path-between compound-entrance workshop)
  (make-path-between murkwater-docks workshop)
  (make-path-between sewers-2 storage-closet)
  (make-path-between storage-closet workshop)
  (make-path-between workshop control-room)
  (make-path-between workshop martaanvuo-source)
  (make-path-between control-room reactor-room)
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
