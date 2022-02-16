#lang at-exp racket

(provide resolve-action!)

(require racket/lazy-require)

(require
  "../round-resolver/event.rkt"
  "../round-resolver/simulation.rkt"
  "../round-resolver/timeline.rkt"

  "../../actions/action.rkt"

  "../../actors/actor.rkt"
  "../../actors/condition.rkt"
  "../../actors/pc-actor.rkt"
  "../../actors/status.rkt"

  "../../blurbs/blurbs.rkt"

  "../../combat/combat-action-resolver.rkt"
  "../../combat/stance.rkt"

  "../../core/checks.rkt"
  "../../core/io.rkt"
  "../../core/utils.rkt"

  "../../locations/0-types/location.rkt"
  "../../locations/0-types/route.rkt"
  "../../locations/locations.rkt"
  "../../locations/routes.rkt" ; not unused

  "../../pc/pc.rkt"

  "../../state/logging.rkt"
  "../../state/state.rkt"

  "../../world/world.rkt" ; not unused

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


(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (rules-to-lambda rules)
  `(λ () ,@rules))

; action-result is either a timeline, a symbol, or void
(define (resolve-action! action)
  (when (actor-alive? (action-actor action))
    (define result 'not-resolved)

    (when (and (pc-actor? (action-actor action))
               (not (pending? action)))

      (define on-before-rules (action-on-before-rules action))
      (when (not (empty? on-before-rules))
        (when (not (procedure? on-before-rules))
          (set! on-before-rules (rules-to-lambda on-before-rules)))
        (define resolution-result ((eval on-before-rules ns)))
        (dev-note (format "PRE-RESULT: ~a" resolution-result))
        (set! result resolution-result)))


    (when (timeline? result)
      (handle-pc-action-interrupted! result)
      (set-pending-action! action (- (action-duration action)
                                     (timeline-duration result)))
      (set! result 'interrupted))

    (when (not (eq? result 'interrupted))

      (define rules (action-resolution-rules action))
      (when (not (empty? rules))
        (when (not (procedure? rules))
          (set! rules (rules-to-lambda rules)))
        (define resolution-result ((eval rules ns)))

        (dev-note (format "RESULT: ~a" resolution-result))

        (when (not (or (void? resolution-result) (empty? resolution-result)))
          (set! result resolution-result)))
      (define duration (action-duration action))
      (define tl (advance-time-until-next-interesting-event! duration #f))
      (process-timeline! tl))



    (when (and (pc-actor? (action-actor action))
               (not (eq? result 'interrupted)))

      (define on-after-rules (action-on-after-rules action))
      (when (not (empty? on-after-rules))
        (when (not (procedure? on-after-rules))
          (set! on-after-rules (rules-to-lambda on-after-rules)))
        (define resolution-result ((eval on-after-rules ns)))
        (dev-note (format "POST-RESULT: ~a" resolution-result))
        (when (not (void? resolution-result))
          (set! result resolution-result)
          )
        ))

    (wait-for-confirm)

    result))


; note: this has overlap with handle-interrupting-event
(define (process-timeline! tl)
  (for ([event (timeline-events tl)])
    (case (event-type event)
      ['new-time-of-day ; proc dailies here
       '()]
      ['not-hungry '()]
      ['hungry '()]
      ['very-hungry '()]
      ['starving '()]
      [else
       (dev-note (format "process-timeline!: unknown event type ~a" (event-type event)))
       '()]))
  (narrate-timeline tl))


(define (set-pending-action! action time-left)
  (define pending-action action)
  (set-action-duration! pending-action time-left)
  (set-action-details! pending-action (append-element (action-details pending-action) 'pending))
  (current-pending-action pending-action))


(define (handle-pc-action-interrupted! timeline)
  (define interrupting-events
    (filter
     (λ (event) (event-interrupting? event))
     (timeline-events timeline)))
  (cond ((eq? (length interrupting-events) 1)
         (define event (car interrupting-events))
         (handle-interrupting-event! event)
         )
        (else
         (dev-note "handle-pc-action-interrupted!: unexpected amount of interrupting events.")))
  )
