#lang at-exp racket

(provide make-limbtearer get-limbtearer-action get-limbtearer-reaction)

(require
  "common.rkt"

  "../../0-engine/0-api/api.rkt"
  )

(define (make-limbtearer)
  (define enemy (make-actor "Limbtearer" 'limbtearer #:max-hp 4 #:size 'large))
  (set-actor-dexterity! enemy 6)
  (set-actor-strength! enemy 14)
  (set-trait! enemy "defense" 1)
  (set-trait! enemy "melee-attack-skill" 1)
  enemy)

(define (fight-behavior actor)
  (cond [(equal? (stance-range (actor-stance actor)) 'engaged)
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
               (cond [(actor-has-condition-of-type? (pc) 'dislocated-shoulder)
                      (notice "The limbtearer rips out Otava's arm.")
                      (take-damage (pc) 2 'major-trauma)
                      (inflict-condition!
                       (pc)
                       (condition 'arm-missing
                                  (current-elapsed-time)
                                  "missing arm"))
                      (inflict-condition!
                       (pc)
                       (condition 'bleeding-profusely
                                  (current-elapsed-time)
                                  "bleeding profusely"))]
                     [else
                      (notice "The limbtearer's grabs Otava's arm and wrangles it behind her back. There's a wet, cracking, tearing sound.")
                      (inflict-condition!
                       (pc)
                       (condition 'dislocated-shoulder
                                  (current-elapsed-time)
                                  "dislocated shoulder"))
                      ]
                     )
               ]
              [else
               (notice "The limbtearer is too far to reach Otava.")
               ])
            '()
            )
          )
         ]
        [else (get-closer-action actor)]
        ))

(define (flee-behavior actor)
  (cond [else
         (cond [(equal? (stance-range (actor-stance actor)) 'far)
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
                     (if (equal? value 'engaged)
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
     (case (get-current-light-level)
      ['pitch-black (get-skip-action actor)]
      [else (fight-behavior actor)])
     ]
    [else (flee-behavior actor)])
  )

(define (get-limbtearer-reaction actor)
  '())
