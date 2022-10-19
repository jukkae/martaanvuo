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

(define (explore-action base-duration roll-result
         )
  (define duration base-duration)
  (make-action
    #:symbol 'explore
    #:actor (pc)
    #:duration duration
    #:tags '(downtime)
    #:resolution-rules
    `(
      (case ',roll-result
       [(,'critical-failure)
        (p "Ground gives way underneath Otava's feet at a rocky incline. She falls down and breaks her ankle.")
        (inflict-condition!
          (pc)
          (FreshInjury 'ankle-broken
            (current-elapsed-time)
            ""
            (current-elapsed-time)
            ))
        (award-xp! 1)]
       [(,'serious-failure)
        (p "Instead of finding anything, Otava gets lost. Finding her way back takes longer than expected.")
        (notice (format "Action duration: ~a" ,duration))
        ]
       [(,'failure)
        (p "Otava finds nothing of interest.")]
       [(,'narrow-success ,'success)
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
                    '()#;(set-Place-explored! (current-location) 'explored)
                    ]
                    [else
                    (set-Place-explored! (current-location) 'partially-explored)])]
            [else (notice "Otava can't find anything.")])
         (wait-for-confirm)
        ]
        [(,'critical-success)
         (define crit-features '())
         (for ([z (location-zones (current-location))])
           (cond [(pc-has-sense-organ? (SenseOrgan-id (Clue-requires (Zone-clue? z)))
                                       (SenseOrgan-level (Clue-requires (Zone-clue? z))))
                  ; TODO: ie., "pc-fulfills-requirements"
                  ]
                 [
                  (append-element! crit-features z)
                  ]
            )
           )
         (cond [(empty? crit-features)]
          (displayln "TODO: add more features"))
         (define f (take-random crit-features))

         (cond
          [(Zone? f)
            (p (format "A lucky strike! Otava chances upon something she wouldn't have: ~a" (Zone-name f)))
            ; TODO: "move-pc-to-zone"
            (for ([z_ (location-zones (current-location))])
              (set-Zone-pc-here?! z_ #f))
            (set-Zone-found?! f #t)
            (set-Zone-clue?! f '())
            (set-Zone-pc-here?! f #t)
            ])
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
    (define roll-result
      (check "2d6" #:title "exploration roll" #:target-number 6 #:bonus '()))
    (explore-action explore-cost roll-result)
    )))
