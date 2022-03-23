#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "0-types/condition.rkt"

         "../core/io.rkt"
         "../core/utils.rkt")

(lazy-require
 ["actor.rkt"
  (take-damage
   )]
 ["../combat/combat-action-resolver.rkt"
  (display-combatant-info
   )]
 ["../state/mutators.rkt"
  (pc
   )])

(define (condition-on-end-round! condition owner-id)
  (case (condition-type condition)
    ['bleeding
     (define target (pc))
     (define roll (d 1 2))
     (notice (format "Bleeding roll: 1d2: ~a" roll))
     (case roll
       [(1) (notice "TAKE DAMAGE")]
       [else (notice "NO DAMAGE")])
     (dev-note "TODO: Bleeding")]
    [else
     (dev-note (format "TODO: Something else: ~a" (condition-type condition)))])
  '())
