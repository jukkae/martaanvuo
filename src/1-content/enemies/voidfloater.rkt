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

(define (get-closer-action actor)
  (define next-range
    (case (stance-range (actor-stance actor))
      ['adjacent 'engaged]
      ['close 'adjacent]
      ['nearby 'close]
      ['far 'nearby]))
  (make-action
   #:symbol 'change-range
   #:actor actor
   #:duration 1
   #:target '()
   #:tags '(initiative-based-resolution)
   #:resolution-rules
   `(
     (displayln "Change range TODO:")
     'ok)
   #:details '(slow)))

(define (fight-behavior actor)
  (cond [(eq? (stance-range (actor-stance actor)) 'engaged)
         (make-action
          #:symbol 'skip
          #:actor actor
          #:duration 0
          #:target '()
          #:tags '(initiative-based-resolution)
          #:details '(slow silent))]
        [else
         (get-closer-action actor)]))

(define (flee-behavior actor)
  (make-action
    #:symbol 'skip
    #:actor actor
    #:duration 0
    #:target '()
    #:tags '(initiative-based-resolution)
    #:details '(slow silent)))

(define (get-voidfloater-action actor)
  (cond
    [(> (actor-hp actor) (/ (actor-max-hp actor) 2))
     (fight-behavior actor)]
    [else (flee-behavior actor)])
  )

(define (get-voidfloater-reaction actor)
  '())
