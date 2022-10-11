#lang at-exp racket

(require
  "../../../1-index/content.rkt"

  "../../../3-types/action.rkt"
  "../../../3-types/choice.rkt"
  "../../../3-types/location.rkt"
  "../../../3-types/place.rkt"
  "../../../3-types/route.rkt"

  "../../../4-systems/checks/checks.rkt"

  "../../../7-state/state.rkt"
  )

(provide make-explore-choice)

(define (make-explore-choice)
  (define explore-cost
    (cond [(equal? (location-size (current-location)) 'large)
           "3d20"]
          [else
           "1d10"]))
  (make-choice
   'explore
   (format "Explore. [~a ι]" explore-cost)
   (λ ()
    (define actual-explore-cost
      (just-roll explore-cost #:title "explore cost"))
    (make-action
          #:symbol 'explore
          #:actor (pc)
          #:duration actual-explore-cost
          #:tags '(downtime)
          #:resolution-rules
          `(
            (define discoverables
              (append (location-hidden-features (current-location))
                      (Place-hidden-routes (current-location))))
            (cond [(not (empty? discoverables))
                   (define discovery (take-random discoverables))
                   (cond
                     [(equal? discovery 'nothing)
                      (notice "Otava finds nothing interesting, but there might still be something.")]
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
                   (notice "Otava can't find anything. She thinks there's nothing to be found anymore.")
                   (set-Place-explored! (current-location) 'exhaustively-explored)
                   ]
                  )
            (wait-for-confirm)
            )
          ))))
