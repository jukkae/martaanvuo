#lang racket

(provide (all-defined-out))

(require racket/serialize)

(require "actor.rkt")
(require "location.rkt")
(require "utils.rkt")

(serializable-struct
 world
 (locations
  day
  [elapsed-time #:mutable]))

(define edgeflats
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '()
   #:type 'edgeflats))

(define swamp
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '(search-for-paths)
   #:type 'swamp))

(define ridges
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '(search-for-paths)
   #:type 'ridges))

(define valleys
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '(search-for-paths)
   #:type 'valleys))

(define crematory
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '()
   #:type 'crematory))

(define ruins
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '()
   #:type 'ruins))

(define sewers
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '()
   #:type 'sewers))

(define cache
  (make-location
   #:actors '()
   #:features '()
   #:items '(u-235 veilbreaker-staff)
   #:neighbors '()
   #:tags '()
   #:actions-provided '()
   #:type 'cache))

(define workshop
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '()
   #:type 'workshop))

(define spring
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '(dive-in-spring)
   #:type 'spring))

(define (setup-world)
  (set-location-neighbors! edgeflats (list swamp))
  (set-location-neighbors! swamp (list edgeflats ridges valleys))
  (set-location-neighbors! ridges (list swamp))
  (set-location-neighbors! valleys (list swamp))
  (set-location-neighbors! crematory (list valleys))
  (set-location-neighbors! ruins (list ridges sewers cache))
  (set-location-neighbors! sewers (list ruins workshop))
  (set-location-neighbors! cache (list ruins))
  (set-location-neighbors! workshop (list sewers spring))
  (set-location-neighbors! spring (list workshop))
  )

(define (expose-neighbor! location)
  (displayln "exposing neighbor")
  (cond ((eq? (location-type location) 'ridges)
         (displayln "location type ridges")
         (set-location-neighbors! ridges (list swamp ruins))
         )
        (else (error "unknown location type"))))

; world-as-simulation / scripting API
(define (remove-actor-from-its-current-location! actor)
  (define current-location (actor-current-location actor))
  (when (not (eq? '() current-location))
    (remove-actor-from-location! current-location actor)))

; world-as-simulation / scripting API
(provide move-actor-to-location!)
(define (move-actor-to-location! actor location)
  (remove-actor-from-its-current-location! actor)
  (set-actor-current-location! actor location)
  (add-actor-to-location! location actor))
