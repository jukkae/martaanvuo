#lang at-exp racket

;; where does this file belong?

(require racket/lazy-require)

(require
  "action.rkt"
  "choice.rkt"

  "../actors/actor.rkt"

  "../blurbs/blurbs.rkt"

  "../combat/combat-pc-choices.rkt"

  "../core/checks.rkt"
  "../core/io.rkt"
  "../core/utils.rkt"

  "../items/item.rkt"

  "../locations/locations.rkt"
  "../locations/0-types/location.rkt"
  "../locations/narration.rkt"

  "../pc/pc.rkt"

  "../state/logging.rkt"
  "../state/state.rkt"

  "../world/time.rkt"
  "../world/world.rkt"
  )


(lazy-require
 ["../resolvers/round-resolver/round-resolver.rkt"
  (go-to-fragment
   )])



; TODO: action / time helpers, move these somewhere
(define (time-until-next-morning)
  (let* ([time (world-elapsed-time (current-world))]
         [time-today (remainder time day-length)])
    (- day-length time-today)))

(define (time-until-next-time-of-day)
  (- 100 (remainder (world-elapsed-time (current-world)) 100)))


(provide get-world-choices)
(define (get-world-choices world actor)
  (cond ((in-combat?)
         (get-combat-choices))
        ((eq? (time-of-day-from-jiffies (world-elapsed-time (current-world))) 'evening)
         (get-evening-choices world actor))
        ((eq? (time-of-day-from-jiffies (world-elapsed-time (current-world))) 'night)
         (get-nighttime-choices world actor))
        (else (get-downtime-choices world actor))))

(define (choice-factory action-symbol)
  (case action-symbol
    ['sleep
     (make-choice
      'sleep
      "Sleep."
      (λ () (make-action
             #:symbol 'sleep
             #:actor (pc)
             #:duration (time-until-next-morning)
             )))]
    ['tent
     (make-choice
      'camp
      "Set up a tent."
      (λ () (make-action
             #:symbol 'camp
             #:actor (pc)
             #:duration 20
             #:resolution-rules
             `(
               (displayln "Camp action TODO")
               'ok))))]

    ['campfire
     (make-choice
      'campfire
      "Build campfire."
      (λ () (make-action
             #:symbol 'camp
             #:actor (pc)
             #:duration 10
             #:resolution-rules
             `(
               (displayln "Campfire action TODO")
               'ok))))]

    ['rest
     (define next-time-of-day
       (time-of-day-from-jiffies (+ (world-elapsed-time (current-world))
                                    100)))
     (make-choice
      'rest
      (format "Rest. [until ~a]" next-time-of-day)
      (λ () (make-action
             #:symbol 'rest
             #:actor (pc)
             #:duration (time-until-next-time-of-day)
             #:resolution-rules
             `(
               (blurb 'rest-action))
             )))]

    ; This opens a submenu
    ['eat
     (define title
       (case (pc-hunger-level)
         ['satiated "Eat? Disgusting idea."]
         ['not-hungry "Not really hungry but she could eat."]
         ['hungry "Eat."]
         ['very-hungry "Eat, she's very hungry."]
         ['starving "She's starving, eat. Eat now."]
         ))
     (make-choice
      'eat
      "Eat."
      (λ ()
        (define food (select-food-to-eat))
        (if (void? food)
            'cancel
            (begin
              (make-action
               #:symbol 'eat
               #:actor (pc)
               #:duration 15
               #:target food
               #:tags '(downtime)
               #:resolution-rules
               `(
                 (define food-tier
                   (case (,item-id ,food)
                     ['fresh-berries 0]
                     ['ration 1]
                     ['vatruska 2]
                     [else
                      (displayln (format "Unknown comestible ~a" (,item-id ,food)))
                      1])
                   )
                 (decrease-pc-hunger-level food-tier)

                 (case (,item-id ,food)
                   ['fresh-berries (p "The berries are invigoratingly sweet.")]
                   ['ration (p "The ration's dry and bland, but filling.")]
                   ['vatruska (p "The vatruska tastes heavenly.")])
                 (remove-item! (,item-id ,food))

                 ))))

        ))
     ]
    ))

(define (select-food-to-eat)
  (define items (actor-inventory (pc)))
  (define comestibles
    (filter (λ (item) ; likely this should be stored as data on the item itself
              (case (item-id item)
                ['fresh-berries #t]
                ['ration #t]
                ['vatruska #t]
                [else #f]))
            items))

  (prln (format "Eat what? [1-~a], anything else to cancel." (length comestibles)))
  (br)

  (for ([food comestibles]
        [i (in-naturals 1)])
    (prln (format "[~a] ~a (~a)" i (item-name food) (item-details food))))
  (br)
  (define input (string->number (wait-for-input)))
  (cond ((and (number? input)
              (> input 0)
              (<= input (length comestibles)))
         (define index (- input 1))
         (list-ref comestibles index)
         )
        (else (p "Nevermind.")))
  )


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
              (reset-pending-action!)))))

       ; route traversal can be canceled
       (when (route? (current-location))
         (define destination
           (get-location-by-id (get-cancel-and-go-back-destination
                                (current-location)
                                (current-pending-action))))
         (make-choice
          'cancel-traverse
          ; the pending action's direction is needed
          (get-cancel-pending-action-and-go-back-name (current-location) (current-pending-action))
          (λ () (make-action
                 #:symbol 'cancel-traverse
                 #:actor (pc)
                 #:duration 100
                 #:target (location-id destination)
                 #:tags '(downtime)
                 #:resolution-rules
                 `(
                   (define from
                     (cond (,(route? destination)
                            (if (memq 'a-to-b (action-details (current-pending-action))) ; TODO check that this is OK
                                (route-a destination)
                                (route-b destination)))
                           (else
                            (current-location))
                           ))

                   (define to
                     (cond (,(route? destination)
                            (if (memq 'a-to-b (action-details (current-pending-action))) ; TODO check that this is OK
                                (route-b destination)
                                (route-a destination)))
                           (else
                            ,destination)
                           ))
                   (reset-pending-action!)
                   (move-pc-to-location! ,destination)

                   (describe-cancel-traverse-action from to)
                   (display-location-info-card (current-location))
                   (when (not (null? (location-items (current-location))))
                     (pick-up-items!))
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
                    (define traverse-duration 100)
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
                                ;(dev-note "TODO: Handle pending actions")
                                (define elapsed-time 0)

                                (when (not (location-has-detail? (current-location) 'no-encounters))
                                  (define encounter-roll (d 1 6))
                                  (notice (format "Encounter roll: 1d6 < 6: [~a] – ~a"
                                                  encounter-roll
                                                  (if (< encounter-roll 6)
                                                      "fail"
                                                      "success")))
                                  (when (< encounter-roll 6)

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
                              (when (not (null? (location-items (current-location))))
                                (pick-up-items!))
                              )

                            )))))
             )))

       (when (and (not (eq? (time-of-day-from-jiffies (world-elapsed-time (current-world))) 'night))
                  (place? (current-location)))
         (list (choice-factory 'rest)))

       (when (and (>= (pc-actor-hunger (current-pc)) 100)
                  (pc-has-item? 'ration))
         (list
          (choice-factory 'eat)))

       (when (not (null? (actor-conditions (pc))))
         (list (make-choice
                'treat-wounds
                "Treat wounds."
                (λ () (make-action
                       #:symbol 'treat-wounds
                       #:actor (pc)
                       #:duration 80
                       #:tags '(downtime)
                       #:resolution-rules
                       `(
                         (for ([c (actor-conditions (pc))])
                           (case (condition-type c)
                             ['ankle-broken
                              (p "Otava splints her purple, swollen ankle. She tries putting a little weight on it and immediately regrets it. There are multiple fractures in the small bones in her ankle.")]
                             ['bleeding
                              (p "Otava bandages her wounds. She's going to have some more scars.")])
                           )
                         )
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
                           (define amount (d 1 4)) ; portions = days of survival
                           (define amount-string
                             (if (= amount 1)
                                 (format "~a meal" amount)
                                 (format "~a meals" amount)))

                           (info-card
                            (tbody
                             (tr
                              "1d4"
                              "="
                              (format "~a" amount-string))
                             )
                            "Forage results roll")
                           (p "After some time, Otava finds some edible fruits and roots. (" (number->string amount) " meals.)")
                           (define item (list 'food (list amount)))
                           (add-item! item)
                           )
                          (else
                           (begin
                             (p "Despite spending a while, Otava can't find anything to eat.")
                             (define luck-roll (d 1 20))
                             (info-card
                              (tbody
                               (tr
                                "1d20"
                                "="
                                (format "~a" luck-roll)))
                              "Luck roll")
                             )))
                    (if successful?
                        'successful
                        'failure)
                    )

                  )))))

       (when (place? (current-location))
         (for/list ([action (place-actions-provided (current-location))])
           (case action
             [else (error (format "get-downtime-choices: unknown action provided ~a" action))])))

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


       (when (and (eq? (location-type (current-location)) 'perimeter)
                  (not (flag-set? 'tried-to-go-back))
                  (= (current-run) 1))
         (choice
          'end-run
          "Go back."
          (make-action
           #:symbol 'end-run
           #:actor actor
           #:duration 100
           #:tags '(downtime)
           #:resolution-rules
           `(
             (cond ((flag-set? 'ending-run-allowed)
                    (p "At least it's something.")
                    'end-run)
                   (else
                    (set-flag 'tried-to-go-back)
                    (p @~a{
 Fuck it. Not worth it, she's not ready yet. Here's the, uh, it was a scouting trip to figure out the route. Which she did.
 })
                    (wait-for-confirm)
                    (next-chapter!) ; end chapter, but not run!
                    (p "Otava is getting close to what she's looking for, but she has trouble remembering how she got here. Did she follow the path of the Mediator? What was it that she was after?")
                    (wait-for-confirm)
                    (p "The Maw, the Monograph, the Cache, and the Gold. A single mind, laser-focused on four targets, one of which is the same as the other, ultimately, just two stages to both. Like, if you think about it, one's a way to freedom, one's a way to freedom, one's a way to a way to freedom, and one's a way to a way to freedom. One's a one way away from... Fucking hippies were right afterall, got to be free, man, 'cause otherwise what's the point? Die a fucking slave? Ha ha.")
                    (p "This should be simple, Otava thinks.")
                    (award-xp! 25 "for good thinking")
                    'failure
                    ))))

          ))

       (when (and (eq? (location-type (current-location)) 'perimeter)
                  (flag-set? 'ending-run-allowed))
         (choice
          'end-run
          "Go back home."
          (make-action
           #:symbol 'end-run
           #:actor actor
           #:duration 100
           #:tags '(downtime)
           #:resolution-rules
           `(
             (cond ((flag-set? 'ending-run-allowed)
                    (p "At least it's something.")
                    'end-run)
                   (else
                    (set-flag 'tried-to-go-back)
                    (p @~a{
 Fuck it. Not worth it, she's not ready yet. Here's the, uh, it was a scouting trip to figure out the route. Which she did.
 })
                    (wait-for-confirm)
                    (next-chapter!) ; end chapter, but not run!
                    (p "Otava is getting close to what she's looking for, but she has trouble remembering how she got here. Did she follow the path of the Mediator? What was it that she was after?")
                    (wait-for-confirm)
                    (p "The Maw, the Monograph, the Cache, and the Gold. A single mind, laser-focused on four targets, one of which is the same as the other, ultimately, just two stages to both. Like, if you think about it, one's a way to freedom, one's a way to freedom, one's a way to a way to freedom, and one's a way to a way to freedom. One's a one way away from... Fucking hippies were right afterall, got to be free, man, 'cause otherwise what's the point? Die a fucking slave? Ha ha.")
                    (p "This should be simple, Otava thinks.")
                    (award-xp! 25 "for good thinking")
                    'failure
                    ))))

          ))
       ))))

  (define condensed (condense all-actions))
  condensed)


; where does this belong? some module auxilliary to round-resolver?
; store in the action, handle calling from here
; -> code to action handler?
(provide describe-pc-intention)
(define (describe-pc-intention pc-action)
  (when (not (null? pc-action)) ; should be checked at call site but eh
    (case (action-symbol pc-action)
      ['forage (p "Otava is getting low on supplies. Too low to be comfortable. Here looks good as any, so she decides to take a look around, see if there's anything edible.")]
      #;[else (p "TBD")])))
