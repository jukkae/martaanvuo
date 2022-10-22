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
   #:symbol 'emblinden
   #:actor actor
   #:duration 1
   #:target target-id
   #:tags '(initiative-based-resolution)
   #:resolution-rules
   `(
     (define target (pc))
     (cond [(pc-has-sense-organ? 'eyes)
            ; TODO: SenseOrgans-to-Descriptions-map? (ordering is case-by-case - order of notices affects dramaturgy!)
            (when (pc-has-sense-organ? 'ears)
              (notice "There's a sharp whoosh. A claw cuts through the air.")
              )
            (when (pc-has-sense-organ? 'nociception)
              (notice "A sharp, burning pain cuts across Otava's eyes.")
              )
            (when (pc-has-sense-organ? 'haptics)
              (notice "Warm blood and intraocular fluid gush on her face.")
              )
            (when (pc-has-sense-organ? 'eyes)
              (when (not (equal? (get-current-light-level) 'pitch-black))
                (notice "A flurry of claws, and then Otava's vision goes black.")
                )
              )
            (remove-sense-organ! 'eyes)
            (wait-for-confirm)
            ])
     )
   ))

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
