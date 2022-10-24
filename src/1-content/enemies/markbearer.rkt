#lang at-exp racket

(provide (all-defined-out))

(require
  "common.rkt"

  "../../0-engine/2-core/io.rkt"
  "../../0-engine/2-core/core.rkt"

  "../../0-engine/3-types/action.rkt"
  "../../0-engine/3-types/actor.rkt"

  "../../0-engine/4-systems/actors/actor.rkt"
  "../../0-engine/4-systems/checks/checks.rkt"

  "../../0-engine/3-types/stance.rkt"

  "../../0-engine/7-state/state.rkt"
  )

(define (make-markbearer)
  (define enemy (make-actor "markbearer" 'markbearer #:max-hp 4 #:size 'large))
  (set-actor-dexterity! enemy 13)
  (set-trait! enemy "defense" 1)
  (set-trait! enemy "melee-attack-skill" 1)
  (set-trait! enemy "visual description" "Looks almost like a human, sneaky, scaly")
  enemy)

(define (make-poke-out-eyes-action actor)
  (define target-id (actor-id (pc)))
  (make-action
   #:symbol 'emblinden
   #:actor actor
   #:duration 1
   #:target target-id
   #:tags '(initiative-based-resolution)
   #:resolution-rules
   `(
     (define target (pc))
     (cond [(pc-has-sense-organ? 'eyes)
            (notice "The markbearer pushes his thumbs hard into Otava's eyes. There's two small pops as Otava's globes rupture.")
            (remove-sense-organ! 'eyes)
            (wait-for-confirm)
            ])
     )
   ))

(define (get-markbearer-action actor)
  (cond
   [(> (actor-hp actor) 2)
    (case (actor-stance-range actor)
     [(engaged adjacent)
      (cond [(equal? (stance-range (actor-stance actor)) 'engaged)
             (define n-of-choices 6)
             (define choice (- (d 1 n-of-choices) 1))
             (case choice
              [(0)
               (make-poke-out-eyes-action actor)
               ]
              [(1 2 3 4 5)
               (make-melee-action actor #:n 1 #:x 1 #:bonus 0)
               ])
             ]
            [else
             (define n-of-choices 6)
             (define choice (- (d 1 n-of-choices) 1))
             (case choice
              [(0 1)
               (make-skip-action actor)
               ]
              [else
               (make-melee-action actor #:n 1 #:x 1 #:bonus 0)
               ]
              )
             ])
       ]
     [else
      (approach-action actor)])
    ]
   [else
    (try-to-escape actor)
    ])
  )

(define (get-markbearer-reaction actor)
  '())
