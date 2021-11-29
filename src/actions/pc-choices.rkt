#lang racket

;; where does this file belong?

(require racket/lazy-require)

(require
  "action.rkt"
  "choice.rkt"
  "../actors/actor.rkt"
  "../combat/combat-pc-choices.rkt"
  "../core/io.rkt"
  "../items/item.rkt"
  "../pc/pc.rkt"
  "../actors/pc-actor.rkt"
  "../locations/location.rkt"
  "../locations/place.rkt"
  "../locations/route.rkt"
  "../locations/narration.rkt"
  "../world/time.rkt"
  "../core/utils.rkt"
  "../world/world.rkt"
  "../state/state.rkt")

(lazy-require
 ["../martaanvuo.rkt"
  (actor-in-range?
   move-actor-to-location!
   )])

(lazy-require
 ["../round-resolver/round-resolver.rkt"
  (go-to-story-fragment
   )])

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
             #:duration 200)))]
    ['tent
     (make-choice
      'camp
      "Set up a tent."
      (λ () (make-action
             #:symbol 'camp
             #:actor (pc)
             #:duration 20)))]

    ['campfire
     (make-choice
      'camp
      "Build campfire."
      (λ () (make-action
             #:symbol 'camp
             #:actor (pc)
             #:duration 10)))]

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
             #:duration 100)))]

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
           (make-action
            #:symbol 'eat
            #:actor (pc)
            #:duration 15
            #:target food
            #:tags '(downtime)))

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
           (get-cancel-and-go-back-destination
            (current-location)
            (current-pending-action)))
         (make-choice
          'cancel-traverse
          ; the pending action's direction is needed
          (get-cancel-pending-action-and-go-back-name (current-location) (current-pending-action))
          (λ () (make-action
                 #:symbol 'cancel-traverse
                 #:actor (pc)
                 #:duration 100
                 #:target destination
                 #:tags '(downtime)))))

       (when (and (not (in-combat?))
                  (not (location-has-tag? (current-location) 'forbid-simple-exit)))

         (when (place? (current-location))
           (for/list ([route (place-routes (current-location))])

             (define direction
               (cond ((eq? (location-id (current-location))
                           (location-id (route-a route)))
                      'a-to-b)
                     ((eq? (location-id (current-location))
                           (location-id (route-b route)))
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
                          (make-empty-action))))
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
                           #:tags '(downtime))))))
                    )
                   (else ; route is traversable
                    (make-choice
                     'traverse
                     (get-traverse-text route (current-location)) 
                     (λ () (make-action
                            #:symbol 'traverse
                            #:actor (pc)
                            #:duration 100
                            #:target route
                            #:tags '(downtime)
                            #:details (list direction))))))
             )))

       (when (and (not (eq? (time-of-day-from-jiffies (world-elapsed-time (current-world))) 'night))
                  (place? (current-location)))
         (list (choice-factory 'rest)))

       (when (and (>= (pc-actor-hunger (current-pc)) 100)
                  (pc-has-item? 'ration))
         (list
          (choice-factory 'eat)))

       (when (eq? (location-type (current-location)) 'swamp)
         (list
          (make-choice
           'forage
           "Forage."
           (λ () (make-action
                  #:symbol 'forage
                  #:actor (pc)
                  #:duration 100
                  #:tags '(downtime))))))

       (when (place? (current-location))
         (for/list ([action (place-actions-provided (current-location))])
           (case action
             ['search-for-paths
              (make-choice
               'search-for-paths
               "Search for paths."
               (λ () (make-action
                      #:symbol 'search-for-paths
                      #:actor (pc)
                      #:duration 100
                      #:tags '(downtime))))]
             [else (error (format "get-downtime-choices: unknown action ~a" action))])))

       (filter
        (λ (x) (and (not (null? x))
                    (not (void? x))))
        (for/list ([feature (location-features (current-location))])
          (case feature
            ['hartmann-device
             (make-choice
              'turn-on-device
              "Turn on Hartmann Device."
              (λ ()
                (p "The fabric of reality begins unfolding itself. The reaction bubbles outwards faster than lightspeed, obliterating all traces of Otava within a nanosecond, and proceeding to blink the entire Universe out of existence.")
                (end-game)))]

            ['magpie-effigy
             (make-choice
              'follow-the-magpie
              "Magpie."
              (λ ()
                (p "Despite the worsening rain, Otava goes into the monochrome bush.")
                (go-to-story-fragment 'magpie)
                (remove-feature-from-location! (current-location) 'magpie-effigy)
                'end-chapter)) ; ie., 'end-round-early, plus next chapter on next round

             ]

            ['precipice
             (make-choice
              'fall-down
              "Check out the mesmerising overhang."
              (λ ()
                (p "The glowing fog is starting to coalesce around her, as a rock gets loose under her foot. Otava slips.")
                (go-to-story-fragment 'fall-down)
                (wait-for-confirm)
                'end-chapter)) ; ie., 'end-round-early, plus next chapter on next round

             ]

            ['anthill
             (make-choice
              'anthill
              "Take a closer look at the anthill."
              (λ ()
                (go-to-story-fragment 'anthill-1)
                'end-chapter)) ; ie., 'end-round-early, plus next chapter on next round

             ]

            [else (dev-note (format "unknown feature ~a" feature))])))

       (when (eq? (location-type (current-location)) 'spring)
         (make-choice
          'dive-in-spring
          "Dive in the spring."
          (λ () (make-action
                 #:symbol 'win-game
                 #:actor (pc)
                 #:duration 0
                 #:tags '(downtime)))))

       (when (and (eq? (location-type (current-location)) 'perimeter)
                  (not (flag-set? 'tried-to-go-back))
                  (= (current-run) 1))
         (make-pc-choice
          #:id 'end-run
          #:text "Turn back."
          #:duration 0
          #:tags '(downtime)))

       (when (and (eq? (location-type (current-location)) 'perimeter)
                  (flag-set? 'ending-run-allowed))
          (make-pc-choice
          #:id 'end-run
          #:text "Go back to the Shack and the Collector."
          #:duration 0
          #:tags '(downtime)))
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
