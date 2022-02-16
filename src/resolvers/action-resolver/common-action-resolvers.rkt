#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require
  "../round-resolver/event.rkt"
  "../round-resolver/simulation.rkt"
  "../round-resolver/timeline.rkt"

  "../../actions/action.rkt"

  "../../actors/actor.rkt"
  "../../actors/pc-actor.rkt"

  "../../combat/combat-action-resolver.rkt"

  "../../core/io.rkt"
  "../../core/utils.rkt"

  "../../locations/0-types/location.rkt"
  "../../locations/0-types/route.rkt"
  "../../locations/locations.rkt"

  "../../state/state.rkt"

  )

(lazy-require
 ["../round-resolver/event-handler.rkt"
  (handle-interrupting-event!
   )])

(lazy-require
 ["../../locations/narration.rkt"
  (describe-begin-traverse-action
   describe-finish-traverse-action
   describe-cancel-traverse-action
   display-location-info-card
   )])

(lazy-require
 ["../../locations/locations.rkt"
  (move-pc-to-location!
   )])



(define (resolve-go-to-action! action)
  (define from
    (cond ((route? (action-target action))
           (if (memq 'a-to-b (action-details action))
               (route-a (action-target action))
               (route-b (action-target action))))
          (else
           (current-location))
          ))

  (define to
    (cond ((route? (action-target action))
           (if (memq 'a-to-b (action-details action))
               (route-b (action-target action))
               (route-a (action-target action))))
          (else
           (action-target action))
          ))
  (define elapsed-time 0)
  (cond ((not (pending? action))
         (describe-begin-traverse-action from to)))

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

        (describe-finish-traverse-action from to)

        (when (not (null? (location-items (action-target action))))
          (pick-up-items!))

        'ok)
      ))
  result)
