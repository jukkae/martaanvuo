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

(define (make-human-fighter)
  (define enemy (make-actor "Human fighter" 'human-fighter #:max-hp 4 #:size 'large))
  (set-actor-dexterity! enemy 13)
  (set-trait! enemy "defense" 1)
  (set-trait! enemy "melee-attack-skill" 1)
  enemy)

(define (get-human-fighter-action actor)
  (cond
   [(> (actor-hp actor) 2)
    (case (actor-stance-range actor)
     [(engaged adjacent)
      (cond [#f
             '()
             ]
            [else
             ((take-random (list
                            make-melee-action
                            ))
              actor)])
       ]
     [else
      (approach-action actor)])
    ]
   [else
    (try-to-escape actor)
    ])
  )

(define (get-human-fighter-reaction actor)
  '())
