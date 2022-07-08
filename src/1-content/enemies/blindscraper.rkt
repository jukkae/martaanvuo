#lang at-exp racket

(provide (all-defined-out))

(require
  "../../0-engine/2-core/io.rkt"
  "../../0-engine/2-core/core.rkt"

  "../../0-engine/3-types/action.rkt"
  "../../0-engine/3-types/actor.rkt"

  "../../0-engine/4-systems/actors/actor.rkt"
  "../../0-engine/4-systems/checks/checks.rkt"

  "../../0-engine/6-combat/stance.rkt"

  "../../0-engine/7-state/state.rkt"
  )

(define (make-blindscraper)
  (define enemy (make-actor "Blindscraper" 'blindscraper 3))
  (set-actor-dexterity! enemy 13)
  (set-trait! enemy "defense" 1)
  (set-trait! enemy "melee-attack-skill" 1)
  (set-trait! enemy "size" "small")
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
        (p "The blindscraper swings its claw through an opening between Otava's arms. The claw tears diagonally across Otava's face, cutting its way through flesh, scraping bone.")
         (wait-for-confirm)
         (case (d 1 2)
           [(1)
            ; -> next generation: scars where there were wounds, then next: tattoos -> with both giving changes to the build - "the ghost that lived through" (it's often possible to name a reason)
            (p "A searing pain cuts through her left eye. Blood and intraocular fluid gush down her face.")
            (inflict-condition! (pc)
                                (condition 'one-eye-blind
                                           "One eye blind."))
            ]
           [(2)
            (p "A searing pain cuts through her eyes as her vision goes black.")
            (inflict-condition! (pc)
                                (condition 'both-eyes-blind
                                           "Blind."))
            ])
        )
      )]

    [else
     (error (format "make-blindscraper-action: unknown action: ~a" action-flag))]))

(define (get-blindscraper-action actor)
  (cond ((in-combat?)
         (cond
           ((> (actor-hp actor) 1)

            (cond
              ((actor-in-range? actor 'engaged)
               (define options
                 (list
                  (cons 1 'blindscrape)
                  (cons 2 'attack)
                  (cons 3 'attack)
                  (cons 4 'attack)))
               (define roll (d 1 4))
               (define index (- roll 1))
               #;(displayln "Action")
               (define action-flag-with-index (list-ref options index))
               #;(displayln action-flag-with-index)
               (define action-flag (cdr action-flag-with-index))
               (make-blindscraper-action actor action-flag))

              ((actor-in-range? actor 'close)
               (define options
                 (list
                  (cons 1 'attack)
                  (cons 2 'attack)
                  (cons 3 'go-to-engaged)
                  (cons 4 'go-to-engaged)
                  #;(cons 4 'parry)
                  ))
               (define roll (d 1 4))
               (define index (- roll 1))
               #;(displayln "Action")
               (define action-flag-with-index (list-ref options index))
               #;(displayln action-flag-with-index)
               (define action-flag (cdr action-flag-with-index))
               (make-blindscraper-action actor action-flag))

              (else
               (define options
                 (list
                  (cons 1 'attack)
                  (cons 2 'attack)
                  (cons 3 'go-to-engaged)
                  (cons 4 'go-to-engaged)
                  #;(cons 4 'parry)
                  ))
               (define roll (d 1 4))
               (define index (- roll 1))
               #;(displayln "Action")
               (define action-flag-with-index (list-ref options index))
               #;(displayln action-flag-with-index)
               (define action-flag 'go-to-close)
               (make-blindscraper-action actor action-flag))))

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
               (define value (get-stance-range-numeric-value (stance-range stance)))
               (define target-number
                 (if (= value 0)
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
               )))))
        (else
         (begin (dev-note "Blindscraper AI, not in combat")))))

(define (get-blindscraper-reaction actor)
  '())
