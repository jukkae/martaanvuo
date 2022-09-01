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

; TODO: get-closer vs. get-further should be resolved with an opposing roll
(define (get-closer-action actor)
  (define next-range
    (case (stance-range (actor-stance actor))
      ['adjacent 'engaged]
      ['close 'adjacent]
      ['nearby 'close]
      ['far 'nearby]))
  (define subject (actor-id actor))
  (make-action
   #:symbol 'get-closer
   #:actor actor
   #:duration 1
   #:target '()
   #:tags '(initiative-based-resolution)
   #:resolution-rules
   `(
     (set-stance-range! (actor-stance (get-actor ,subject)) ',next-range)
     (notice (format "The ~a is now ~a." (actor-name (get-actor ,subject)) (stance-range (actor-stance (get-actor ,subject)))))
     'ok)
   #:details '(slow)))

; TODO: opposed roll
(define (get-further-action actor)
  (define next-range
    (case (stance-range (actor-stance actor))
      ['engaged 'adjacent]
      ['adjacent 'close]
      ['close 'nearby]
      ['nearby 'far]))
  (define subject (actor-id actor))
  (make-action
   #:symbol 'get-further
   #:actor actor
   #:duration 1
   #:target '()
   #:tags '(initiative-based-resolution)
   #:resolution-rules
   `(
     (set-stance-range! (actor-stance (get-actor ,subject)) ',next-range)
     (notice (format "The ~a is now ~a." (actor-name (get-actor ,subject)) (stance-range (actor-stance (get-actor ,subject)))))
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
  (cond [(eq? (stance-range (actor-stance actor)) 'far)
         (make-action
          #:symbol 'skip
          #:actor actor
          #:duration 0
          #:target '()
          #:tags '(initiative-based-resolution)
          #:details '(slow silent))]
        [else
         (get-further-action actor)]))

(define (get-voidfloater-action actor)
  (cond
    [(> (actor-hp actor) (/ (actor-max-hp actor) 2))
     (fight-behavior actor)]
    [else (flee-behavior actor)])
  )

(define (get-voidfloater-reaction actor)
  '())
