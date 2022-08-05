#lang at-exp racket

(provide (all-defined-out))

(require
  "../../0-engine/2-core/io.rkt"
  "../../0-engine/2-core/core.rkt"

  "../../0-engine/3-types/action.rkt"
  "../../0-engine/3-types/actor.rkt"

  "../../0-engine/4-systems/actors/actor.rkt"
  "../../0-engine/4-systems/checks/checks.rkt"

  "../../0-engine/3-types/stance.rkt"

  "../../0-engine/7-state/state.rkt"
  )

(define (make-voidfloater)
  (define enemy (make-actor "Voidfloater" 'voidfloater #:max-hp 3 #:size 'small))
  (set-actor-dexterity! enemy 13)
  (set-trait! enemy "defense" 1)
  (set-trait! enemy "melee-attack-skill" 1)
  enemy)

(define (get-voidfloater-action actor)
  (make-action
    #:symbol 'skip
    #:actor actor
    #:duration 0
    #:target '()
    #:tags '(initiative-based-resolution)
    #:details '(slow silent)))

(define (get-voidfloater-reaction actor)
  '())
