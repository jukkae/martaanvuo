#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "0-types/condition.rkt"

         "../core/io.rkt"
         "../core/utils.rkt"
         )

(lazy-require
 ["actor.rkt"
  (take-damage
   )]
 ["../combat/combat.rkt"
  (display-combatant-info
   )]
 ["../state/mutators.rkt"
  (pc
   )]
 ["../world/world.rkt"
  (get-actor)])

(define (condition-on-end-round! condition owner-id)
  (define owner (get-actor owner-id))
  (case (condition-type condition)
    ['bleeding
     (define bleed-damage-roll (d 1 6)) ; could give bonus from constitution here? say, 1d6?
     (cond ((= 1 bleed-damage-roll)
            (notice "Bleed check: 1d6 = 1: [1] => 1 dmg")
            (take-damage owner 1 'bleed)
            (display-combatant-info owner)
            )
           (else
            (notice (format "Bleed check: 1d6 = 1: [~a]" bleed-damage-roll))))]
    [else
     (dev-note (format "TODO: Something else: ~a" (condition-type condition)))])
  '())
