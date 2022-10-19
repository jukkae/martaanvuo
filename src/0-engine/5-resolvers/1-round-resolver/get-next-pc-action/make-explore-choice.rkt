#lang at-exp racket

(require
  "../../../1-index/content.rkt"

  "../../../2-core/io.rkt"

  "../../../3-types/action.rkt"
  "../../../3-types/choice.rkt"
  "../../../3-types/location.rkt"
  "../../../3-types/place.rkt"
  "../../../3-types/route.rkt"

  "../../../4-systems/checks/checks.rkt"

  "../../../7-state/state.rkt"
  )

(provide make-explore-choice)

(define (explore-action base-duration exploration-roll
         )
  (define duration base-duration)
  (define success-level '())
  (case exploration-roll
    [(2)
     (set! success-level 'critical-failure)]
    [(3)
    (set! duration (* 3 base-duration))
    (set! success-level 'serious-failure) ; TODO: type?
    ]
    [(4 5 6)
    (set! success-level 'failure)
    ]
    [(7 8 9 10 11 12)
    (set! success-level 'success)
    ]
    )
  (make-action
    #:symbol 'explore
    #:actor (pc)
    #:duration duration
    #:tags '(downtime)
    #:resolution-rules
    `(
      (case ',success-level
       [(,'critical-failure)
        (p "Ground gives way underneath Otava's feet at a rocky incline. She falls down and breaks her ankle.")
        (inflict-condition!
          (pc)
          (current-elapsed-time)
          (FreshInjury 'ankle-broken ""
            (current-elapsed-time)
            ))]
       [(,'serious-failure)
        (p "Instead of finding anything, Otava gets lost. Finding her way back takes longer than expected.")
        (notice (format "Action duration: ~a" ,duration))
        ]
       [(,'failure)
        (p "Otava finds nothing of interest.")]
       [(,'success)
        (p "Otava finds something.")

        (define discoverables
        (append (location-hidden-features (current-location))
                (Place-hidden-routes (current-location))))
        (cond [(not (empty? discoverables))
              (define discovery (take-random discoverables))
              (cond
                [(not (null? (get-route-by-id discovery)))
                (define discovered-route (get-route-by-id discovery))
                (set-route-hidden?! (get-route-by-id discovery) #f)
                (notice (format "Otava finds a route: ~a"
                  (route-description-from discovered-route (current-location))))
                ]
                [else
                (add-feature-to-location! (current-location) discovery)
                (notice (format "New discovery: ~a" discovery))
                ])

              (remove-hidden-feature-from-location! (current-location) discovery)
              (cond [(empty? (location-hidden-features (current-location)))
                    (set-Place-explored! (current-location) 'explored)
                    ]
                    [else
                    (set-Place-explored! (current-location) 'partially-explored)])]
            [else
              (notice "Otava can't find anything, and she's looked *everywhere*.")
              (set-Place-explored! (current-location) 'exhaustively-explored)
              ]
            )
          (wait-for-confirm)
        ]
       )
      )
    )
  )

(define (make-explore-choice)
  (define explore-cost
    (cond [(equal? (location-size (current-location)) 'large)
           30]
          [else
           10]))
  (make-choice
   'explore
   (format "Explore. [~a ι]" explore-cost)
   (λ ()
    (define exploration-roll
      (just-roll "2d6"
       #:title "exploration roll [2d6 >= 7] "
       #:on-critical-failure
       (λ () (displayln "crit failure!"))
       #:on-critical-success (λ () (displayln "crit success!"))
       ))
    (explore-action explore-cost exploration-roll)
    )))
