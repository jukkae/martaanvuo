#lang at-exp racket

(require racket/lazy-require)

(require
  "choice-factory.rkt"
  "make-cancel-traverse-choice.rkt"
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
  #;(when (location-has-feature? (current-location) 'the-bell)
    (append-element! feature-choices
                     (make-choice
                      'ring-new-enemy-bell
                      "Ring the bell"
                      `(
                        (spawn-encounter)
                        '()
                        )))
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
                         #:tags (list 'initiative-based-resolution)
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
  (append (filter (lambda (c) (choice-available-in-combat? c))
                          feature-choices)
          (cond ((in-combat?)
                 (append
                  (get-combat-choices)
                  (filter (lambda (c) (choice-available-in-combat? c))
                          (get-current-location-choices))
                  (cond [(not (null? (location-items (current-location))))
                         (list (choice-factory 'pick-up-item))]
                        [else '()])
                  )
                 
                 )
                [else
                 (define world-choices '())
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
  #;(dev-note "get-night-time-choices: not implemented yet")
  (list
   (choice-factory 'sleep)))

(define (get-evening-choices world actor)
  #;(dev-note "get-evening-choices: not implemented yet")
  (prune (list
  ;  (choice-factory 'tent)
  ;  (choice-factory 'campfire)
   (when (and (>= (pc-actor-hunger (current-pc)) hunger-level-hungry)
                  (pc-has-item? 'ration)) ; TODO: should check for all comestibles
         (choice-factory 'eat))
   (choice-factory 'rest)
   )))


(define (get-downtime-choices world actor)
  (define (show-based-on-pending-choice? choice)
    (if (null? (current-pending-action))
        #t
        (begin
          (cond
            ; show pending action
            ((string-prefix? (choice-name choice) "[continue]")
             #t)

            ; don't show actions that have same symbol as pending action
            ; (note: this may or may not lead to intended results, see how it works)
            ; plot twist: it is shit and has to be fixed
            ((equal? (choice-id choice) (action-symbol (current-pending-action)))
             #f)

            ; show anything else
            (else
             #t)))))

  (define all-actions

    (filter
     (λ (x) (show-based-on-pending-choice? x))
     (condense
      (list

       (when (not (null? (current-pending-action)))
         (make-choice
          (action-symbol (current-pending-action))
          (get-continue-pending-action-name)

          (λ ()
            (begin0
              (current-pending-action)
              ; continue and reset
              (reset-pending-action!)))))

       ; route traversal can be canceled
       (when (route? (current-location))
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
                    (make-traverse-choice route direction)
                            ))
             )))

       (when (and (not (equal? (time-of-day-from-iotas (world-elapsed-time (current-world))) 'night))
                  (Place? (current-location)))
         (list (choice-factory 'rest)))

       (when (and (>= (pc-actor-hunger (current-pc)) hunger-level-hungry)
                  (pc-has-item? 'ration)) ; TODO: should check for all comestibles
         (list
          (choice-factory 'eat)))

       (when (Place? (current-location))
        (when (or (equal? (Place-explored (current-location)) '())
                 (equal? (Place-explored (current-location)) 'not-explored))
         (list (make-choice
                'explore
                "Explore."
                (λ () (make-action
                       #:symbol 'explore
                       #:actor (pc)
                       #:duration 5
                       #:tags '(downtime)
                       #:resolution-rules
                       `(
                         (set-Place-explored! (current-location) 'partially-explored)
                         )
                       )))))
       (when (equal? (Place-explored (current-location)) 'partially-explored)
         (list (make-choice
                'explore-more
                "Explore more."
                (λ () (make-action
                       #:symbol 'explore-more
                       #:actor (pc)
                       #:duration 10
                       #:tags '(downtime)
                       #:resolution-rules
                       `(
                         (set-Place-explored! (current-location) 'explored)
                         )
                       )))))

       (when (equal? (Place-explored (current-location)) 'explored)
         (list (make-choice
                'explore-even-more
                "Explore even more."
                (λ () (make-action
                       #:symbol 'explore-even-more
                       #:actor (pc)
                       #:duration 10
                       #:tags '(downtime)
                       #:resolution-rules
                       `(
                         (define result (just-roll "d100" #:title "Exploration"))
                         (notice (format "Total: ~a" result))
                         (cond
                          [(= result 99)
                           (notice "The place is now exhaustively explored.")
                           (set-Place-explored! (current-location) 'exhaustively-explored)]
                          [else
                           (notice "Otava doesn't find anything interesting.")])
                         (wait-for-confirm)
                         )
                       ))))))

       (when (not (null? (actor-conditions (pc))))
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
          #:duration 100
          #:tags '(downtime)
          #:resolution-rules
          `(
            @p{
===================
NOTES ON PROTOZOAN-
INVERTEBRATE EVOLUTION
AND EVIDENCE OF PATH-
OLOGICAL SELF-ORGANIZATION IN
===================
,
the cover of the notebook says, handwritten. The full title didn't fit in the small white area. Otava flips the book open.
              }
            (wait-for-confirm)
            (set-flag 'scenario-evolution)
            'recurse
            )

          ))))
