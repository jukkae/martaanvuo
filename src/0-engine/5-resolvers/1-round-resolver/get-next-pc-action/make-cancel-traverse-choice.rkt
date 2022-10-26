#lang at-exp racket

(require "../../../1-index/content.rkt"

         "../../../3-types/action.rkt"
         "../../../3-types/choice.rkt"
         "../../../3-types/location.rkt"
         "../../../3-types/place.rkt"
         "../../../3-types/route.rkt"

         "../../../4-systems/world/world.rkt"
         "../../../4-systems/actors/conditions.rkt"

         "../../../7-state/state.rkt")

(provide make-cancel-traverse-choice)
(define (make-cancel-traverse-choice)
  (define destination
    (get-location-by-id
     (get-cancel-and-go-back-destination (current-location) (current-pending-action))))

  (define cancel-action-duration (exact-floor (/ (route-traverse-time (current-location)) 2)))
  (when (actor-has-condition-of-type? (pc) 'ankle-broken)
    (set! cancel-action-duration (* 3 cancel-action-duration)))

  (define cancel-action-direction
    (if (equal? (get-pending-traverse-direction) 'a-to-b) 'b-to-a 'a-to-b))

  (make-choice
   'cancel-traverse
   ; the pending action's direction is needed
   (format "~a [~a ι]"
           (get-cancel-pending-action-and-go-back-name (current-location) (current-pending-action))
           cancel-action-duration)
   (λ ()
     (make-action
      #:symbol 'cancel-traverse
      #:actor (pc)
      #:duration cancel-action-duration
      #:target (location-id destination)
      #:tags '(downtime)
      #:on-before-rules
      `((define tl (advance-time-until-next-interesting-event! ,cancel-action-duration #f)) tl)
      #:resolution-rules `((define from
                             (cond
                               [,(route? destination)
                                (if (memq 'a-to-b (action-details (current-pending-action)))
                                    (route-a destination)
                                    (route-b destination))]
                               [else (current-location)]))
                           (define destination-id ',(location-id destination))
                           (define to
                             (cond
                               [,(route? destination)
                                (if (memq 'a-to-b (action-details (current-pending-action)))
                                    (route-b destination)
                                    (route-a destination))]
                               [else (get-location-by-id destination-id)]))
                           (notice (format "~a FOOBAR" (timestamp)))
                           (move-pc-to-location! (get-location-by-id destination-id))
                           (describe-cancel-traverse-action from to)
                           (reset-pending-action!)
                           (display-location-info-card (current-location))
                           'ok)))))
