#lang at-exp racket

(require racket/lazy-require)

(require
  "choice-factory.rkt"

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


  "../../../4-rules/actors/actor.rkt"
  "../../../4-rules/blurbs/blurbs.rkt"
  "../../../4-rules/checks/checks.rkt"
  "../../../4-rules/items/item.rkt"
  "../../../4-rules/locations/locations.rkt"
  "../../../4-rules/pc/pc.rkt"
  "../../../4-rules/world/time.rkt"
  "../../../4-rules/world/world.rkt"

  "../../../6-combat/combat-pc-choices.rkt"

  "../../../7-state/state/logging.rkt"
  "../../../7-state/state/state.rkt"
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
         (define destination
           (get-location-by-id (get-cancel-and-go-back-destination
                                (current-location)
                                (current-pending-action))))
           (define cancel-action-duration (exact-floor (/ (action-duration (current-pending-action)) 2)))
         (make-choice
          'cancel-traverse
          ; the pending action's direction is needed
          (get-cancel-pending-action-and-go-back-name (current-location) (current-pending-action))
          (λ () (make-action
                 #:symbol 'cancel-traverse
                 #:actor (pc)
                 #:duration cancel-action-duration
                 #:target (location-id destination)
                 #:tags '(downtime)
                 #:resolution-rules
                 `(
                   (define from
                     (cond [,(route? destination)
                            (if (memq 'a-to-b (action-details (current-pending-action)))
                                (route-a destination)
                                (route-b destination))]
                           [else
                            (current-location)]
                           ))

                   (define destination-id ',(location-id destination))
                   (define to
                     (cond [,(route? destination)
                            (if (memq 'a-to-b (action-details (current-pending-action)))
                                (route-b destination)
                                (route-a destination))]
                           [else
                            (get-location-by-id destination-id)]
                           ))

                   (move-pc-to-location! (get-location-by-id destination-id))
                   (describe-cancel-traverse-action from to)
                   (reset-pending-action!)
                   (display-location-info-card (current-location))
                   'ok

                   )


                 ))))

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
                    (when (not (pc-has-ammo-left?))
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
                    (define traverse-duration (route-traverse-time route))
                    (make-choice
                     'traverse
                     (get-traverse-text route (current-location))
                     (λ () (make-action
                            #:symbol 'traverse
                            #:actor (pc)
                            #:duration traverse-duration
                            #:target (location-id route)
                            #:tags '(downtime)
                            #:details (list direction)
                            #:resolution-rules
                            `(
                              (set-route-traversed! (get-route-by-id ',(location-id route)))

                              (define next-location-id
                                ',(if (eq? direction 'a-to-b)
                                      (route-b route)
                                      (route-a route)))
                              (define next-location (get-location-by-id next-location-id))
                              (move-pc-to-location! next-location)
                              'ok
                              )
                            #:on-before-rules
                            `(
                              (let/ec return
                                (describe-begin-traverse-action (get-route-by-id ',(location-id route)) ',direction)
                                (define next-location (get-route-by-id ',(location-id route)))
                                (move-pc-to-location! next-location)

                                (define elapsed-time 0)

                                (when (not (location-has-detail? (current-location) 'no-encounters))
                                  (define encounter-roll (d 1 6))
                                  (define tn 3)
                                  (notice (format "Encounter roll: 1d6 < ~a: [~a] – ~a"
                                                  encounter-roll
                                                  tn
                                                  (if (< encounter-roll tn)
                                                      "fail"
                                                      "success")))
                                  (when (< encounter-roll tn)

                                    (define resolve-events
                                      (list
                                       (make-event ,''spawn-enemies
                                                   '() ; pack info about enemies / event here
                                                   #:interrupting? #t)))
                                    (define metadata '(interrupted))
                                    (define duration
                                      (exact-floor (/
                                                    ,traverse-duration
                                                    3)))

                                    (set! elapsed-time duration)

                                    (define world-tl (advance-time-until-next-interesting-event! duration #f))
                                    (define world-events (timeline-events world-tl))

                                    (define all-events (append world-events resolve-events))
                                    (define all-metadata (append (timeline-metadata world-tl) metadata))

                                    (define tl (timeline all-metadata all-events duration))

                                    (process-timeline! tl)
                                    (return tl))
                                  )

                                'before-action-ok
                                ))
                            #:on-after-rules
                            `(
                              (describe-finish-traverse-action (get-route-by-id ',(location-id route)) ',direction)
                              )

                            )))))
             )))

       (when (and (not (eq? (time-of-day-from-iotas (world-elapsed-time (current-world))) 'night))
                  (place? (current-location)))
         (list (choice-factory 'rest)))

       (when (and (>= (pc-actor-hunger (current-pc)) 100)
                  (pc-has-item? 'ration))
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

       (when (eq? (location-type (current-location)) 'swamp)
         (list
          (make-choice
           'forage
           "Forage."
           (λ () (make-action
                  #:symbol 'forage
                  #:actor (pc)
                  #:duration 100
                  #:tags '(downtime)
                  #:resolution-rules
                  `(
                    (define skill 0)
                    (define target-number 8)

                    (define successful? (skill-check "Forage" skill target-number))
                    (cond (successful?
                           (define amount (d 1 2))
                           (define amount-string
                             (if (= amount 1)
                                 (format "~a handful" amount)
                                 (format "~a handfuls" amount)))

                           (info-card
                            (tbody
                             (tr
                              "1d4"
                              "="
                              (format "~a" amount-string))
                             )
                            "Forage results roll")
                           (p "Otava finds crowberries and bogberries. (" (number->string amount) " handfuls.)")
                           (define item (make-item 'fresh-berries #:amount amount))
                           (add-item! item)
                           )
                          )
                    (if successful?
                        'successful
                        'failure)
                    )

                  )))))

       (filter
        (λ (x) (and (not (null? x))
                    (not (void? x))))
        (for/list ([feature (location-features (current-location))])
          (case feature
            ['magpie-effigy
             (make-choice
              'follow-the-magpie
              "Magpie."
              (λ ()
                (p "Despite the worsening rain, Otava goes into the monochrome bush.")
                (go-to-fragment 'magpie)
                (remove-feature-from-location! (current-location) 'magpie-effigy)
                'end-chapter)) ; ie., 'end-round-early, plus next chapter on next round

             ]

            ['precipice
             (make-choice
              'fall-down
              "Check out the mesmerising overhang."
              (λ ()
                (p "The glowing fog is starting to coalesce around her, as a rock gets loose under her foot. Otava slips.")
                (go-to-fragment 'fall-down)
                (wait-for-confirm)
                'end-chapter)) ; ie., 'end-round-early, plus next chapter on next round

             ]

            ['anthill
             (cond [(not (flag-set? 'anthill-seen))
                    (make-choice
                     'anthill
                     "Anthill."
                     (λ ()
                       (set-flag 'anthill-seen)
                       (go-to-fragment 'anthill-1)
                       'end-chapter ; ie., 'end-round-early, plus next chapter on next round
                       ))]
                   [else

                    ; TODO: This should be known and decided by in "content related to anthill" – for instance perhaps in fragments/anthill?
                    (define next-anthill-fragment
                      (cond [(pc-has-item? 'grabberkin-finger)
                             'anthill-complete-fingers]
                            [else 'anthill-2]))

                    (make-choice
                     'anthill
                     "Back to Anthill."
                     (λ ()
                       (go-to-fragment next-anthill-fragment)
                       'end-round-early
                       ))])
             ]

            ['waiting-room-begin
             (make-choice
              'waiting-room
              "Enter the waiting room."
              (λ ()
                (p "The penultimate step towards Ascending to a Higher Plane of Existence: To enter the waiting room!")
                (go-to-fragment 'waiting-room-1)
                'end-chapter)) ; ie., 'end-round-early, plus next chapter on next round

             ]

            [else (dev-note (format "unknown feature ~a" feature))])))


       (when (eq? (location-id (current-location)) 'perimeter)
         (get-perimeter-choices))
       ))))

  (define condensed (condense all-actions))
  condensed)
