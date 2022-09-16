#lang at-exp racket

(provide (all-defined-out))

(require
  anaphoric

  "common.rkt"

  "../../0-engine/2-core/io.rkt"
  "../../0-engine/2-core/core.rkt"

  "../../0-engine/3-types/action.rkt"
  "../../0-engine/3-types/actor.rkt"

  "../../0-engine/4-systems/actors/actor.rkt"
  "../../0-engine/4-systems/checks/checks.rkt"
  "../../0-engine/4-systems/pc/pc.rkt"

  "../../0-engine/3-types/stance.rkt"

  "../../0-engine/7-state/state.rkt"
  )

(define (make-blindscraper)
  (define enemy (make-actor
    "Blindscraper"
    'blindscraper
    #:max-hp 3
    #:size 'small
    ))
  (set-actor-dexterity! enemy 13)
  (set-trait! enemy "defense" 1)
  (set-trait! enemy "melee-attack-skill" 1)
  enemy)


(define (make-blindscrape-action actor)
  (define target-id (actor-id (pc)))
  (make-action
   #:symbol 'inflict-condition
   #:actor actor
   #:duration 1
   #:target target-id
   #:tags '(initiative-based-resolution)
   #:resolution-rules
   `(
     (define target (pc))
     (notice "The blindscraper swings its claw through an opening between Otava's arms. The claw tears across Otava's face, cutting its way through flesh, scraping bone.")
     (wait-for-confirm)
     (cond [(pc-has-sense-organ? 'eyes)
            (remove-sense-organ! 'eyes)
            ])
     )
   ))

(define (make-melee-action actor)
  (make-melee-attack-action
   #:actor actor
   #:duration 1
   #:target 'pc
   #:n 1
   #:x 2
   #:bonus -1
   )
  )

(define (get-blindscraper-action actor)
  ; TODO: hunt by sound/sonar + smell; approach/retreat (collectively called "reposition actions")
  (cond ((in-combat?)
         (cond
           ((> (actor-hp actor) 1)

            (case (actor-stance-range actor)
              [(engaged adjacent)
               (cond [(pc-has-sense-organ? 'eyes)
                      ((take-random (list
                                     make-blindscrape-action
                                     ))
                       actor)
                      ]
                     [else
                      ((take-random (list
                                     make-melee-action
                                     ))
                       actor)])
               ]
              [else
               (approach-action actor)])

            )

           ((= (actor-hp actor) 1)
            (try-to-escape actor))
           ))
        ))

(define (get-blindscraper-reaction actor)
  '())
