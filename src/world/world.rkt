#lang at-exp racket


(require racket/lazy-require)

(require "0-types/world.rkt"
         "content/world.rkt")

(require "../actors/actor.rkt"
         "../items/item.rkt"
         "../locations/0-types/location.rkt"
         "../core/utils.rkt")

(lazy-require ["../state/state.rkt"
               (current-world
                current-location)])

; API
(define (get-current-actors)
  (define actors (location-actors (current-location)))
  actors)

(provide get-actor)
(define (get-actor id)
  (define actors (get-current-actors))
  (findf (λ (a) (eq? (actor-id a) id)
           actors))
  )

; world-as-simulation / scripting API
(provide remove-actor-from-its-current-location!)
(define (remove-actor-from-its-current-location! actor)
  (define current-loc (get-location-by-id (actor-location-id actor)))

  (when (not (null? current-loc))
    (remove-actor-from-location! current-loc actor))
  (set-actor-location-id! actor '()))

; world-as-simulation / scripting API
(provide move-actor-to-location!)
(define (move-actor-to-location! actor location)
  (remove-actor-from-its-current-location! actor)
  (set-actor-location-id! actor (location-id location))
  (add-actor-to-location! location actor))

(provide (all-from-out
          "0-types/world.rkt"
          "content/world.rkt"))
