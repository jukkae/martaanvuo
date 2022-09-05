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
  (define subject (actor-id actor))
  (make-action
   #:symbol 'get-closer
   #:actor actor
   #:duration 1
   #:target '()
   #:tags '(initiative-based-resolution)
   #:resolution-rules
   `(
     (set-actor-stance-range! (get-actor ,subject) ',next-range #f)
     'ok)
   #:details '(slow)))

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
     (set-actor-stance-range! (get-actor ,subject) ',next-range)
     'ok)
   #:details '(slow)))

(define (fight-behavior actor)
  (cond [(eq? (stance-range (actor-stance actor)) 'engaged)
         (define target-id (actor-id (pc)))
         (make-melee-attack-action
          #:actor actor
          #:duration 1
          #:target target-id
          #:n 1
          #:x 2
          #:bonus 0
          )]
        [else
         (get-closer-action actor)]))

(define (flee-behavior actor)
  (cond [(eq? (stance-range (actor-stance actor)) 'far)
         ; TODO: extract function
         (define id (actor-id actor))
         (make-action
          #:symbol 'escape
          #:actor actor
          #:duration 1
          #:target '()
          #:tags '(initiative-based-resolution fast)
          #:details '()
          #:resolution-rules
          `(
            (notice (format "~a tries to escape." (get-combatant-name (get-actor ,id))))
            (define skill 1)
            (define stance (actor-stance (get-actor ,id)))
            (define value (stance-range stance))
            (define target-number
              (if (eq? value 'engaged)
                  10
                  8))

            ; (define success? (skill-check "Athletics" skill target-number))
            (define success? #t)
            (if success?
                (begin
                  (notice "The voidfloater escapes.")
                  (award-xp! 1)
                  (remove-actor-from-its-current-location! (get-actor ,(actor-id actor)))
                  'ok)
                (begin
                  'failure))
            ))]
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
