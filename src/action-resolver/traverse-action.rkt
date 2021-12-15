#lang racket

(provide
 resolve-cancel-traverse-action!
 resolve-traverse-action!
 resolve-go-to-action!)

(require racket/lazy-require)

(require
  "../actions/action.rkt"

  "../locations/0-types/location.rkt"
  "../locations/0-types/route.rkt"
  "../locations/locations.rkt"

  "../round-resolver/simulation.rkt"
  "../round-resolver/timeline.rkt"

  "../state/state.rkt"
  )

(lazy-require
 ["../locations/narration.rkt"
  (describe-begin-traverse-action
   describe-finish-traverse-action
   describe-cancel-traverse-action
   display-location-info-card
   )])

(define (resolve-cancel-traverse-action! action)
  (reset-pending-action!)
  (move-pc-to-location! (action-target action))

  (describe-cancel-traverse-action action)
  (display-location-info-card (current-location))
  (when (not (null? (location-items (action-target action))))
    (pick-up-items!))
  'ok)


(define (resolve-traverse-action! action)
  (set-route-traversed! (action-target action))

  (define next-location (if (memq 'a-to-b (action-details action))
                            (route-b (action-target action))
                            (route-a (action-target action))))
  (move-pc-to-location! next-location)

  'ok)


(define (resolve-go-to-action! action)
  (define elapsed-time 0)
  (cond ((not (pending? action))
         (describe-begin-traverse-action action)))

  (define result
    (let/ec return
      (begin
        ; begin advancing time
        (define timeline
          (advance-time-until-next-interesting-event! (action-duration action) #f)) ; no encounters
        (set! elapsed-time (timeline-duration timeline))

        #;(narrate-timeline timeline)

        (when (eq? (timeline-metadata timeline) 'interrupted)
          (return timeline))


        (define next-location (action-target action))
        (move-pc-to-location! next-location)
        (location-on-enter! (current-location))

        (describe-finish-traverse-action action)

        (when (not (null? (location-items (action-target action))))
          (pick-up-items!))

        'ok)
      ))
  result)
