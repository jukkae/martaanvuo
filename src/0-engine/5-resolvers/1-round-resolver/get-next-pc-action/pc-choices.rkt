#lang at-exp racket

(require racket/lazy-require)

(require
  "choice-factory.rkt"
  "make-choices-based-on-features.rkt"
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
  (cond ((in-combat?)
         (get-combat-choices))
        [else
         (define world-choices '())
         (when (not (null? (location-items (current-location))))
                           (set! world-choices (append-element world-choices (choice-factory 'pick-up-item))))
         (cond [(eq? (time-of-day-from-iotas (world-elapsed-time (current-world))) 'evening)
                (set! world-choices (append world-choices (get-evening-choices world actor)))]
               [(eq? (time-of-day-from-iotas (world-elapsed-time (current-world))) 'night)
                (set! world-choices (append world-choices (get-nighttime-choices world actor)))]
               [else (set! world-choices (append world-choices (get-downtime-choices world actor)))])
         world-choices
        ]))


(define (get-nighttime-choices world actor)
  (dev-note "get-night-time-choices: not implemented yet")
  (list
   (choice-factory 'sleep)))

(define (get-evening-choices world actor)
  (dev-note "get-evening-choices: not implemented yet")
  (list
   (choice-factory 'tent)
   (choice-factory 'campfire)
   (choice-factory 'eat)
   (choice-factory 'rest)
   ))


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
            ((eq? (choice-symbol choice) (action-symbol (current-pending-action)))
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
         (choice
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

         (when (place? (current-location))
           (for/list ([route-id (place-routes (current-location))])
             (define route (get-route-by-id route-id))
             (define direction
               (cond ((eq? (location-id (current-location))
                           (route-a route))
                      'a-to-b)
                     ((eq? (location-id (current-location))
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

       (when (and (not (eq? (time-of-day-from-iotas (world-elapsed-time (current-world))) 'night))
                  (place? (current-location)))
         (list (choice-factory 'rest)))

       (when (and (>= (pc-actor-hunger (current-pc)) hunger-level-hungry)
                  (pc-has-item? 'ration)) ; TODO: should check for all comestibles
         (list
          (choice-factory 'eat)))

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

       (when (and (eq? (location-type (current-location)) 'swamp)
                  (not (once-per-day-action-done? 'forage)))
         (list
          (make-forage-choice)))

       (make-choices-based-on-features)


       (when (eq? (location-id (current-location)) 'perimeter)
         (get-perimeter-choices))
       ))))

  (define condensed (condense all-actions))
  condensed)
