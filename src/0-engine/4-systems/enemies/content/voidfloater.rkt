#lang at-exp racket

(provide (all-defined-out))

(require
  "../../actors/actor.rkt"
  "../../checks/checks.rkt"

  "../../../2-core/io.rkt"
  "../../../2-core/core.rkt"
  "../../../3-types/action.rkt"
  "../../../3-types/actor.rkt"
  "../../../6-combat/stance.rkt"
  "../../../7-state/state.rkt"
  )

(define (make-voidfloater)
  (define enemy (make-actor "Voidfloater" 'voidfloater 3))
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
