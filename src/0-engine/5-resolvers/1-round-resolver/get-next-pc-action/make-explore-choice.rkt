#lang at-exp racket

(require
  "../../../1-index/content.rkt"

  "../../../3-types/action.rkt"
  "../../../3-types/choice.rkt"
  "../../../3-types/location.rkt"
  "../../../3-types/place.rkt"
  "../../../3-types/route.rkt"

  "../../../7-state/state.rkt"
  )

(provide make-explore-choice)

(define (make-explore-choice)
  (define explore-cost
    (cond [(equal? (location-size (current-location)) 'large)
           30]
          [else
           5]))
  (make-choice
   'explore
   (format "Explore. [~a ι]" explore-cost)
   (λ () (make-action
          #:symbol 'explore
          #:actor (pc)
          #:duration explore-cost
          #:tags '(downtime)
          #:resolution-rules
          `(
            (when (not (empty? (location-hidden-features (current-location))))
              (define discovery (first (location-hidden-features (current-location))))
              (cond
                    [(equal? discovery 'route-to-shack)
                     (add-route-between! 'magpie-hill 'shack 40 'ext)
                     (notice "Otava finds a route to a shack.")]
                    [(equal? discovery 'route-to-pond-of-drowning)
                     (add-route-between! 'magpie-hill 'pond-of-drowning 60 'ext)
                     (notice "Otava finds a route to a small pond.")]
                    [else
                     (add-feature-to-location! (current-location) discovery)
                     (notice (format "New discovery: ~a" discovery))
                     ])

              (remove-hidden-feature-from-location! (current-location) discovery)
              )
            (cond [(empty? (location-hidden-features (current-location)))
                   (set-Place-explored! (current-location) 'explored)
                   ]
                  [else
                   (set-Place-explored! (current-location) 'partially-explored)])
            (wait-for-confirm)
            )
          ))))
