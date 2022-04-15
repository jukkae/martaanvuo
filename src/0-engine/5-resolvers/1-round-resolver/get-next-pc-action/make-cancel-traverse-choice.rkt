#lang at-exp racket

(require
  "../../../1-index/content.rkt"

  "../../../3-types/action.rkt"
  "../../../3-types/choice.rkt"
  "../../../3-types/location.rkt"
  "../../../3-types/route.rkt"

  "../../../4-systems/world/world.rkt"

  "../../../7-state/state.rkt"
  )

(provide make-cancel-traverse-choice)
(define (make-cancel-traverse-choice)
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
