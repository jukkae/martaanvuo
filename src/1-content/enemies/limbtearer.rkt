#lang at-exp racket

(provide make-limbtearer get-limbtearer-action get-limbtearer-reaction)

(require
  "../../0-engine/2-core/io.rkt"
  "../../0-engine/2-core/core.rkt"

  "../../0-engine/3-types/action.rkt"
  "../../0-engine/3-types/actor.rkt"

  "../../0-engine/4-systems/actors/actor.rkt"
  "../../0-engine/4-systems/pc/pc.rkt"

  "../../0-engine/3-types/stance.rkt"

  "../../0-engine/7-state/state.rkt"
  )

(define (make-limbtearer)
  (define enemy (make-actor "Limbtearer" 'limbtearer #:max-hp 4 #:size 'large))
  (set-actor-dexterity! enemy 6)
  (set-actor-strength! enemy 14)
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
   #:duration 0
   #:target '()
   #:tags '(initiative-based-resolution)
   #:resolution-rules
   `(
     (set-actor-stance-range! (get-actor ,subject) ',next-range #f)
     'ok)
   #:details '(slow)))

(define (get-skip-action actor)
  (make-action
   #:symbol 'skip
   #:actor actor
   #:duration 0
   #:target '()
   #:tags '(initiative-based-resolution)
   #:details '(slow silent)))

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
   #:duration 0
   #:target '()
   #:tags '(initiative-based-resolution)
   #:resolution-rules
   `(
     (set-actor-stance-range! (get-actor ,subject) ',next-range)
     'ok)
   #:details '(slow)))

(define (fight-behavior actor)
  (case (stance-range (actor-stance actor))
           ['engaged
            (define target-id (actor-id (pc)))
            (define subject-id (actor-id actor))
            (make-action
             #:symbol 'tear-limb
             #:actor actor
             #:duration 1
             #:target 'pc
             #:tags '(initiative-based-resolution) ; TODO: what to do with non-initiative-based actions in queue? just cancel?
             #:resolution-rules
             `(
               ; check *current* range, at time of resolving action
               (case (stance-range (actor-stance (get-actor ,subject-id)))
                ['engaged
                 (notice "The limbtearer's grabs Otava's arm and wrangles it behind her back. There's a wet, cracking, tearing sound.")
                 (inflict-condition!
                  (pc)
                  (condition 'dislocated-shoulder
                            (current-elapsed-time)
                            "hand unusable"))
                 ]
                [else
                 (notice "The limbtearer is too far to reach Otava.")
                 ])
               '()
               )
             )]
           [else (get-closer-action actor)]
           ))

(define (flee-behavior actor)
  (cond [else
         (cond [(eq? (stance-range (actor-stance actor)) 'far)
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
                         (notice "The limbtearer escapes.")
                         (award-xp! 1)
                         (remove-actor-from-its-current-location! (get-actor ,(actor-id actor)))
                         'ok)
                       (begin
                         'failure))
                   ))]
               [else
                (get-further-action actor)])
         ]))

(define (get-limbtearer-action actor)
  (cond
    [(> (actor-hp actor) (/ (actor-max-hp actor) 3))
     (fight-behavior actor)]
    [else (flee-behavior actor)])
  )

(define (get-limbtearer-reaction actor)
  '())
