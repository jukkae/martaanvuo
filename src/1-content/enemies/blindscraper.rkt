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

(define (make-blindscraper-action actor action-flag)
  (case action-flag

    ['attack
     (define target-id (actor-id (pc)))
     (make-melee-attack-action
      #:actor actor
      #:duration 1
      #:target target-id
      #:n 1
      #:x 2
      #:bonus 0
      )]

    ['go-to-engaged
     (define target-id (actor-id (pc)))
     (make-action
      #:symbol 'go-to-engaged
      #:actor actor
      #:duration 1
      #:target target-id
      #:tags '(initiative-based-resolution)
      #:details '()
      #:resolution-rules
      `(
        (define lp (pc-actor-lp (pc)))
        (define dex ,(actor-dexterity actor))
        (define success?
          (cond ((positive? lp)
                 (notice "LP positive")
                 (attribute-check "Dexterity" dex))
                (else #t)))

        (define old-stance (actor-stance (get-actor ,(actor-id actor))))
        (define sign (stance-sign old-stance))
        (define description (stance-description old-stance))
        (if success?
            (begin
              (p "The Blindscraper suddenly leaps forward and gets a hold of Otava's forearm with a couple of its lanky fingers. One of its long claws is swinging free, looking for an opening.")

              (let ([enemy-stance (stance sign 'engaged description)])
                (set-actor-stance! (get-actor ,(actor-id actor)) enemy-stance)))

            (begin
              (p "The Blindscraper leaps at Otava, but she dives under it and stumbles back to her feet.")
              (notice "-1 LP")
              (set-pc-actor-lp! (pc)
                                (- (pc-actor-lp (pc))
                                   1))
              (when (< (pc-actor-lp (pc)) 0)
                (set-pc-actor-lp! (pc)
                                  0))
              (notice (format "~a" (pc-actor-lp (pc))))
              'failure))
        'ok
        )
      )]

    ['go-to-close
     (define target-id (actor-id (pc)))
     (make-action
      #:symbol 'go-to-close
      #:actor actor
      #:duration 1
      #:target target-id
      #:tags '(initiative-based-resolution)
      #:details '()
      #:resolution-rules
      `(
        (define lp (pc-actor-lp (pc)))
        (define dex ,(actor-dexterity actor))

        (define old-stance (actor-stance (get-actor ,(actor-id actor))))
        (define sign (stance-sign old-stance))
        (define description (stance-description old-stance))

        (p "The Blindscraper skitters towards Otava.")

        (let ([enemy-stance (stance sign 'close description)])
          (set-actor-stance! (get-actor ,(actor-id actor)) enemy-stance))
        'ok
        ))]

    ['blindscrape
     (make-blindscrape-action actor)]

    [else
     (error (format "make-blindscraper-action: unknown action: ~a" action-flag))]))

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

               (define success? (skill-check "Athletics" skill target-number))
               (if success?
                   (begin
                     (p "The blindscraper skitters away and disappears in the foliage.")
                     (award-xp! 1)
                     (remove-actor-from-its-current-location! (get-actor ,(actor-id actor)))
                     'ok)
                   (begin
                     (p "The blindscraper tries to run away, its legs skittering and slipping, but it is not fast enough.")
                     (actor-add-status! (get-actor ,(actor-id actor)) (status 'fallen 1))
                     (display-combatant-info (get-actor ,(actor-id actor)))
                     'failure))
               )))

           ))
        ))

(define (get-blindscraper-reaction actor)
  '())
