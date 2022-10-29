#lang at-exp racket

(require "../../../1-index/content.rkt"

         "../../../2-core/io.rkt"

         "../../../3-types/action.rkt"
         "../../../3-types/choice.rkt"
         "../../../3-types/location.rkt"
         "../../../3-types/place.rkt"
         "../../../3-types/route.rkt"

         "../../../4-systems/actors/conditions.rkt"
         "../../../4-systems/checks/checks.rkt"
         "../../../4-systems/pc/pc.rkt"
         "../../../4-systems/world/world.rkt"

         "../../../7-state/state.rkt")

(provide make-explore-choice)

(define (explore-action base-duration)
  (define duration base-duration)
  (make-action
   #:symbol 'explore
   #:actor (pc)
   #:duration duration
   #:tags '(downtime)
   #:on-before-rules
   `((for ([z_ (location-zones (current-location))])
       (set-Zone-pc-here?! z_ #f)) ; TODO: this should also be done when leaving the location!
     (advance-time-until-next-interesting-event! (/ ,duration 2) #t))
   #:on-after-rules `((advance-time-until-next-interesting-event! (/ ,duration 2) #t))
   #:resolution-rules
   `((define roll-result (check "2d6" #:title "exploration roll" #:target-number 6 #:bonus '()))
     (case roll-result

       [(,'critical-failure)
        (award-xp! 1)
        (p
         "Ground gives way underneath Otava's feet at a rocky incline. She falls down and breaks her ankle.")
        (inflict-condition! (pc)
                            (FreshInjury 'ankle-broken
                                         (current-elapsed-time)
                                         "movement 3x slower"
                                         (current-elapsed-time)))]

       [(,'serious-failure)
        (p
         "Instead of finding anything, Otava gets lost. Finding her way back takes longer than expected.")
        (notice (format "Action duration: ~a" ,duration))]

       [(,'failure) (p "Otava finds nothing of interest.")]

       [(,'narrow-success ,'success)
        (define discoverables
          (append (location-hidden-features (current-location))
                  (Place-hidden-routes (current-location))))
        (define hidden-zones '())
        (for/list ([z (location-zones (current-location))])
          (when (and (null? (Zone-clue? z))
                     (not (Zone-found? z)))
            (append-element! hidden-zones z)))
        (set! discoverables (append discoverables hidden-zones))

        (cond
          [(not (empty? discoverables))
           (define discovery (take-random discoverables))
           (cond
             [(Zone? discovery)
              (notice (format "~a A discovery: ~a" (timestamp) (Zone-name discovery)))
              ; TODO: "move-pc-to-zone"
              (set-Zone-found?! discovery #t)
              (set-Zone-clue?! discovery '())
              (set-Zone-pc-here?! discovery #t)
              (set-Place-explored! (current-location) 'partially-explored)]
             [(not (null? (get-route-by-id discovery)))
              (define discovered-route (get-route-by-id discovery))
              (set-route-hidden?! (get-route-by-id discovery) #f)
              (notice (format "Otava finds a route: ~a"
                              (route-description-from discovered-route (current-location))))]
             [else
              (add-feature-to-location! (current-location) discovery)
              (notice (format "New discovery: ~a" discovery))])
           (set-Place-explored! (current-location) 'partially-explored)

           (when (symbol? discovery)
             (remove-hidden-feature-from-location! (current-location) discovery))
           (cond
             [(empty? (location-hidden-features (current-location)))
              '()
              #;(set-Place-explored! (current-location) 'explored)]
             [else (set-Place-explored! (current-location) 'partially-explored)])]
          [else
           (notice "Otava can't find anything.")
           ; TODO: this logic is not correct (w.r.t hidden zones etc - all should be consolidated!)
           (set-Place-explored! (current-location) 'explored)])
        (wait-for-confirm)]

       [(,'critical-success)
        (define crit-features '())
        (for ([z (location-zones (current-location))])
          (cond
            [(not (null? (Zone-clue? z)))
             (cond
               [(pc-has-sense-organ? (SenseOrgan-id (Clue-requires (Zone-clue? z)))
                                     (SenseOrgan-level (Clue-requires (Zone-clue? z))))
                ; TODO: ie., "pc-fulfills-requirements"
                '()]
               [else (append-element! crit-features z)])]
            [else
             (cond
               [(not (Zone-found? z)) (append-element! crit-features z)])]))
        (cond
          [(empty? crit-features)]
          [displayln "TODO: add more features"])
        (define f (take-random crit-features))

        (cond
          [(Zone? f)
           (notice (format "~a A lucky strike! Otava chances upon something she wouldn't have: ~a"
                           (timestamp)
                           (Zone-name f)))
           ; TODO: "move-pc-to-zone"
           (set-Zone-found?! f #t)
           (set-Zone-clue?! f '())
           (set-Zone-pc-here?! f #t)
           (set-Place-explored! (current-location) 'partially-explored)])])
     'time-passing-handled)))

(define (make-explore-choice)
  (cond
   [(and (not (null? (current-pending-action)))
         (equal? (action-symbol (current-pending-action)) 'explore)
         ;
         (or (and (pc-has-sense-organ? 'eyes)
                  (not (equal? (get-current-light-level) 'pitch-black)))
             (pc-has-sense-organ? 'sonar))
    )

    (make-choice
         (action-symbol (current-pending-action))
         (get-continue-pending-action-name)

         (λ ()
           (begin0
             (current-pending-action)
             ; continue and reset
             (reset-pending-action!))))
    ]
   [else
    (define explore-cost
      (cond
        [(equal? (location-size (current-location)) 'large) 30]
        [else 10]))

    (cond
      ; (pc-has-condition? 'ankle-broken) ; TODO: more ergonomic
      [(actor-has-condition-of-type? (pc) 'ankle-broken) (set! explore-cost (* 3 explore-cost))])

    (cond [(and (Place? (current-location))
                (not (equal? (Place-explored (current-location)) 'exhaustively-explored))
                ;
                (or (and (pc-has-sense-organ? 'eyes)
                         (not (equal? (get-current-light-level) 'pitch-black)))
                    (pc-has-sense-organ? 'sonar)))
           (make-choice 'explore (format "Explore. [~a ι]" explore-cost) (λ () (explore-action explore-cost)))]
          [else
           (make-unavailable-choice "Explore." "Too dark.")])
    ]))
