#lang at-exp racket

(provide resolve-action-initiative)

(require
  "../../2-core/core.rkt"
  "../../3-types/action.rkt"
  "../../3-types/actor.rkt"
  )

; (: -> Action Actor Natural) ; or maybe Initiative instead of Natural
(define (resolve-action-initiative action actor)
  (define dexterity-mod (get-attribute-modifier-for (actor-dexterity actor)))

  (define action-mod 0)

  (cond ((has-tag? action 'fast)
         (set! action-mod 2))
        ((has-tag? action 'slow)
         (set! action-mod -4)))

  (define dice-1 (d 1 6))
  (define dice-2 (d 1 6))

  (define total (+ dice-1 dice-2 action-mod dexterity-mod))
  total)
