#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require
  "../round-resolver/event.rkt"
  "../round-resolver/timeline.rkt"

  "../../1-index/content.rkt"

  "../../2-core/io.rkt"
  "../../2-core/core.rkt"

  "../../3-types/action.rkt"
  "../../3-types/location.rkt"

  "../../4-systems/actors/actor.rkt"
  "../../4-systems/locations/locations.rkt"
  "../../4-systems/simulation.rkt"

  "../../6-combat/combat-action-resolver.rkt"

  "../../7-state/state/state.rkt"
  )

(lazy-require ["../round-resolver/event-handler.rkt"
  (handle-interrupting-event!
   )])

(lazy-require ["../../4-systems/locations/locations.rkt"
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
