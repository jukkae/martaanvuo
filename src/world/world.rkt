#lang at-exp racket


(require racket/lazy-require
         racket/serialize)

(require "0-types/world.rkt"
         "content/world.rkt")

(require "../actors/actor.rkt"
         "../items/item.rkt"
         "../locations/0-types/location.rkt"
         "../locations/0-types/place.rkt"
         "../locations/0-types/route.rkt"
         "../core/utils.rkt")

(lazy-require ["../state/state.rkt" (current-world)])


; world-as-simulation / scripting API
(provide remove-actor-from-its-current-location!)
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

(provide (all-from-out
          "0-types/world.rkt"
          "content/world.rkt"))
