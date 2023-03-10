#lang at-exp racket

(require racket/lazy-require)

(require
  "choice-factory.rkt"
  "make-cancel-traverse-choice.rkt"
  "make-explore-choice.rkt"
  "make-forage-choice.rkt"
  "make-traverse-choice.rkt"

  "../../../1-index/content.rkt"

  "../../../2-core/io.rkt"
  "../../../2-core/core.rkt"

  "../../../3-types/action.rkt"
  "../../../3-types/actor.rkt"
  "../../../3-types/choice.rkt"
  "../../../3-types/item.rkt"
  "../../../3-types/location.rkt"
  "../../../3-types/place.rkt"
  "../../../3-types/pc-actor.rkt"
  "../../../3-types/route.rkt"
  "../../../3-types/world.rkt"


  "../../../4-systems/actors/actor.rkt"
  "../../../4-systems/blurbs/blurbs.rkt"
  "../../../4-systems/checks/checks.rkt"
  "../../../4-systems/items/item.rkt"
  "../../../4-systems/locations/locations.rkt"
  "../../../4-systems/pc/pc.rkt"
  "../../../4-systems/world/time.rkt"
  "../../../4-systems/world/world.rkt"

  "../../../6-combat/combat-pc-choices.rkt"

  "../../../7-state/logging.rkt"
  "../../../7-state/state.rkt"
  )


(lazy-require ["../round-resolver.rkt"
  (go-to-fragment
   )])

(provide get-world-choices)
(define (get-world-choices world actor)
  (define feature-choices '())
  (when (location-has-feature? (current-location) 'the-button)
    (append-element! feature-choices
                     (make-choice
                      'push-the-release-the-subject-button
                      "Release the subject."
                      `(
                        (spawn-encounter)
                        '()
                        )
                      ))
    )
  (when (location-has-feature? (current-location) 'running-centrifuge)
    (append-element! feature-choices
                     (make-choice
                      'shut-down-centrifuge
                      "Shut down the centrifuge."
                      (λ ()
                        (make-action
                         #:symbol 'shut-down-centrifuge
                         #:actor (pc)
                         #:duration 1
                         #:target '()
                         #:tags (list 'initiative-based-resolution)
                         #:details (list 'fast)
                         #:resolution-rules
                         `(
                           (remove-feature-from-location! (current-location) 'running-centrifuge)
                           (add-feature-to-location! (current-location) 'inactive-centrifuge)
                           (notice (format "Noise level is now ~a." (get-current-noise-level)))
                           )
                         ))
                      #:available-in-combat? #t
                      ))
    )
  (when
    (and (not (false? (current-zone)))
      (zone-has-feature? (current-zone) 'cocoon-effigy))
    (append-element! feature-choices
      (cond [(pc-has-item? 'voidfloater-corpse)
                 (make-choice
                  'make-an-offering
                  "Make an offering to the cocoon effigy."
                  (λ ()
                    (p "Otava offers a voidfloater corpse to the cocoon effigy and kneels to receive the Gift.")
                    (remove-item! 'voidfloater-corpse)
                    (randomize-pc-senses!)
                    (wait-for-confirm)
                    '()
                    ))
                 ]
                [else
                 (make-unavailable-choice
                  "Make an offering to the cocoon effigy."
                  "The effigy demands a voidfloater corpse.")]
                 )))
  (when
    (and (not (false? (current-zone)))
      (zone-has-feature? (current-zone) 'ritual-circle))
    (append-element! feature-choices
      (make-choice
        'ritual-of-translocation
        "Perform the Ritual of Translocation to enter the Maw."
        (λ ()
          (move-pc-to-location! (get-location-by-id 'the-maw))
          (p "Otava is now in the Maw.")
          (wait-for-confirm)
          '()
          ))))
  (when (location-has-feature? (current-location) 'inactive-centrifuge)
    (append-element! feature-choices
                     (make-choice
                      'start-centrifuge
                      "Start the centrifuge."
                      (λ ()
                        (make-action
                         #:symbol 'start-centrifuge
                         #:actor (pc)
                         #:duration 1
                         #:target '()
                         #:tags (list 'initiative-based-resolution)
                         #:details (list 'fast)
                         #:resolution-rules
                         `(
                           (remove-feature-from-location! (current-location) 'inactive-centrifuge)
                           (add-feature-to-location! (current-location) 'running-centrifuge)
                           (notice (format "Noise level is now ~a." (get-current-noise-level)))
                           )
                         ))
                      #:available-in-combat? #t
                      ))
    )
  (when (location-has-feature? (current-location) 'light-switch)
    (define tags (if (current-in-combat?) (list 'initiative-based-resolution) '()))
    (append-element! feature-choices
                     (make-choice
                      'toggle-lights
                      "Toggle lights"
                      (λ ()
                        (make-action
                         #:symbol 'toggle-lights
                         #:actor (pc)
                         #:duration 1
                         #:target '()
                         #:tags tags
                         #:details (list 'fast)
                         #:resolution-rules `(
                                              (case (get-current-light-level)
                                                ['bright
                                                 (set-location-light-level! (current-location) 'dark)
                                                 ]
                                                ['dark
                                                 (set-location-light-level! (current-location) 'pitch-black)
                                                 ]
                                                ['pitch-black
                                                 (set-location-light-level! (current-location) 'bright)
                                                 ]
                                                )
                                              (notice (format "Light level is now ~a." (get-current-light-level)))
                                              )
                         ))
                      #:available-in-combat? #t
                      ))
    )
  (append (filter (lambda (c)
                    (if (current-in-combat?)
                        (choice-available-in-combat? c)
                        #t)
                    )
                  feature-choices)
          (cond ((in-combat?)
                 (append
                  (get-combat-choices)
                  (filter (lambda (c) (choice-available-in-combat? c))
                          (get-current-location-choices))
                  (cond [(not (null? (current-zone-items (current-location))))
                         (list (choice-factory 'pick-up-item))]
                        [else '()])
                  )

                 )
                [else
                 (define world-choices '())
                 (when (not (null? (current-zone-items (current-location))))
                   (set! world-choices (append-element world-choices (choice-factory 'pick-up-item))))
                  (when (not (null? (location-items (current-location))))
                   (set! world-choices (append-element world-choices (choice-factory 'pick-up-item))))
                 (cond [(equal? (time-of-day-from-iotas (world-elapsed-time (current-world))) 'evening)
                        (set! world-choices (append world-choices (get-evening-choices world actor)))]
                       [(equal? (time-of-day-from-iotas (world-elapsed-time (current-world))) 'night)
                        (set! world-choices (append world-choices (get-nighttime-choices world actor)))]
                       [else (set! world-choices (append world-choices (get-downtime-choices world actor)))])
                 world-choices
                 ])
          )
  )


(define (get-nighttime-choices world actor)
  (define r '())
  (append-element! r
    (choice-factory 'sleep))
  r)

(define (get-evening-choices world actor)
  #;(dev-note "get-evening-choices: not implemented yet")
  (prune (list
   (choice-factory 'camp)
   (when (and (>= (pc-actor-hunger (current-pc)) hunger-level-hungry)
                  (pc-has-comestibles?))
         (choice-factory 'eat))
   (choice-factory 'rest)
   (when (and (route? (current-location))
              (not (null? (current-pending-action)))
              (not (equal? (action-symbol (current-pending-action)) 'cancel-traverse))
              )
         (make-cancel-traverse-choice))
   (when (and (not (null? (current-pending-action)))
              (not (equal? (action-symbol (current-pending-action)) 'explore)))
         (make-choice
          (action-symbol (current-pending-action))
          (get-continue-pending-action-name)

          (λ ()
            (begin0
              (current-pending-action)
              ; continue and reset
              (reset-pending-action!)))))
   )))


(define (get-downtime-choices world actor)
  (define (show-based-on-pending-choice? choice)
    (if (null? (current-pending-action))
        #t
        (begin
          (cond
            ; show pending action, except for the ones provided as static
            [(string-prefix? (choice-name choice)
                              "[continue]")
             (if (equal? (choice-id choice) 'explore) #f #t)]

            ; don't show actions that have same symbol as pending action
            ((equal? (choice-id choice) (action-symbol (current-pending-action)))
             #f)

            ; show anything else
            (else
             #t)))))

  (define all-actions ; actions or choices?

    (filter
     (λ (x) (show-based-on-pending-choice? x))
     (condense
      (list

       (when (and (not (null? (current-pending-action))))
         (make-choice
          (action-symbol (current-pending-action))
          (get-continue-pending-action-name)

          (λ ()
            (begin0
              (current-pending-action)
              ; continue and reset
              (reset-pending-action!)))))

       ; route traversal can be canceled
       (when (and (route? (current-location))
                  )
         (make-cancel-traverse-choice))

       (when (and (not (in-combat?))
                  (not (location-has-tag? (current-location) 'forbid-simple-exit)))

         (when (Place? (current-location))
           (for/list ([route-id (Place-routes (current-location))])
             (define route (get-route-by-id route-id))
             (define direction
               (cond ((equal? (location-id (current-location))
                           (route-a route))
                      'a-to-b)
                     ((equal? (location-id (current-location))
                           (route-b route))
                      'b-to-a)))

             (cond ((memq 'locked (location-details route))
                    (when (and (pc-has-item? 'revolver)
                               (not (pc-has-ammo-left?)))
                      (p ; this belongs elsewhere
                       "Out of ammo, can't shoot the lock. Damn."))
                    (list
                     (when (and (pc-has-item? 'revolver)
                                (pc-has-ammo-left?))
                       (make-choice
                        'shoot-the-lock
                        "Shoot the lock."
                        (λ ()
                          (p "A gunshot pierces the still air of the Ruins and echoes through tunnels, as Otava shoots open the lock holding a heavy door. The latch swings open.")
                          (consume-ammo! 1)
                          (remove-detail-from-location! route 'locked)
                          (make-action
                           #:symbol 'skip
                           #:actor (pc)
                           #:duration 0
                           #:tags '(downtime)))))
                     (when (and (pc-has-item? 'bolt-cutters))
                       (make-choice
                        'cut-the-lock
                        "Cut the lock with bolt cutters."
                        (λ ()
                          (p "The crude lock yields to Otava's bolt cutters easily.")
                          (remove-detail-from-location! route 'locked)

                          (make-action
                           #:symbol 'skip
                           #:actor (pc)
                           #:duration 0
                           #:tags '(downtime)
                           )))))
                    )

                   (else ; route is traversable
                    (when (not (route-hidden? route))
                      (if (and (location-has-detail? route 'only-when-small)
                               (location-has-detail? route 'only-when-small))
                          (make-unavailable-choice
                           (get-traverse-text route (current-location))
                           "Otava is too large to fit through.")
                          (make-traverse-choice route direction)
                          )
                      )
                    ))
             )))

       (when (and (not (equal? (time-of-day-from-iotas (world-elapsed-time (current-world))) 'night))
                  (Place? (current-location)))
         (list (choice-factory 'rest)))

       (when (and (>= (pc-actor-hunger (current-pc)) hunger-level-hungry)
                  (pc-has-comestibles?))
         (list
          (choice-factory 'eat)))

       (when (or (actor-has-condition-of-type? (pc) 'bleeding)
                 (actor-has-condition-of-type? (pc) 'broken-bones)
                 (actor-has-condition-of-type? (pc) 'strains))
         (list (make-choice
                'treat-injuries
                "Treat injuries."
                (λ () (make-action
                       #:symbol 'treat-injuries
                       #:actor (pc)
                       #:duration 80
                       #:tags '(downtime)
                       #:resolution-rules
                       `((treat-injuries!))
                       )))))

       (when (and (equal? (location-type (current-location)) 'swamp)
                  (not (once-per-day-action-done? 'forage)))
         (list
          (make-forage-choice)))
       (when (and (pc-has-item? 'notebook)
                  (pc-has-sense-organ? 'eyes))
        (make-read-book-choice))

       (get-current-location-choices)
       ))))

  (define condensed (condense all-actions))
  condensed)

(define (make-read-book-choice)
  (make-choice
   'read-book
   "Read the notebook"
   (λ () (make-action
          #:symbol 'read
          #:actor (pc)
          #:duration 5
          #:tags '(downtime)
          #:resolution-rules
          `(
            @p{
!!! WARNING: DO NOT READ !!!
===================
NOTES ON PROTOZOAN-
INVERTEBRATE EVOLUTION
AND EVIDENCE OF PATH-
OLOGICAL SELF-ORGANIZATION IN
===================
,
the cover of the notebook says, handwritten. The full title didn't fit in the small white area provided. The warning is scribbled on top. Otava flips the cover open.
              }
            (wait-for-confirm)
            (set-flag 'scenario-evolution)
            'recurse
            )

          ))))
