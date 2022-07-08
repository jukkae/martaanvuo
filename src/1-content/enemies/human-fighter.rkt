#lang at-exp racket

(provide (all-defined-out))

(require
  "../../0-engine/2-core/io.rkt"
  "../../0-engine/2-core/core.rkt"

  "../../0-engine/3-types/action.rkt"
  "../../0-engine/3-types/actor.rkt"

  "../../0-engine/4-systems/actors/actor.rkt"
  "../../0-engine/4-systems/checks/checks.rkt"

  "../../0-engine/6-combat/stance.rkt"

  "../../0-engine/7-state/state.rkt"
  )

(define (make-human-fighter)
  (define enemy (make-actor "Human fighter" 'human-fighter 3))
  (set-actor-dexterity! enemy 13)
  (set-trait! enemy "defense" 1)
  (set-trait! enemy "melee-attack-skill" 1)
  (set-trait! enemy "size" "human")
  enemy)

(define (get-human-fighter-action actor)
  (make-action
    #:symbol 'skip
    #:actor actor
    #:duration 0
    #:target '()
    #:tags '(initiative-based-resolution)
    #:details '(slow silent)))

(define (get-human-fighter-reaction actor)
  '())
