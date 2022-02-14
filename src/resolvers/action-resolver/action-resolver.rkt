#lang at-exp racket

(provide resolve-action!)

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
  "../../locations/routes.rkt" ; not unused

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
      (set! result (pc-action-on-before-resolve! action)))

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
      (pc-action-on-after-resolve! action))

    (wait-for-confirm)

    result))

(define (pc-action-on-before-resolve! action)
  (let/ec return
    (case (action-symbol action)
      ['traverse
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
       (cond ((not (pending? action))
              (describe-begin-traverse-action from to))
             (else
              (dev-note "resolving pending action")))
       (move-pc-to-location! (action-target action))
       (define elapsed-time 0)
       ; -> roll-for-encounter or something, more content than code -> belongs elsewhere

       (when (not (location-has-detail? (current-location) 'no-encounters))
         (define encounter-roll (d 1 6))
         (notice (format "Encounter roll: 1d6 < 4: [~a] – ~a" encounter-roll (if (< encounter-roll 4)
                                                                                 "fail"
                                                                                 "success")))
         (when (< encounter-roll 4)

           (define resolve-events
             (list
              (make-event 'spawn-enemies
                          '() ; pack info about enemies / event here
                          #:interrupting? #t)))
           (define metadata '(interrupted))
           (define duration (exact-floor (/ (action-duration action) 3)))

           (set! elapsed-time duration)

           (define world-tl (advance-time-until-next-interesting-event! duration #f))
           (define world-events (timeline-events world-tl))

           (define all-events (append world-events resolve-events))
           (define all-metadata (append (timeline-metadata world-tl) metadata))

           (define tl (timeline all-metadata all-events duration))

           (process-timeline! tl)
           (return tl)))

       'before-action-ok
       ]
      [else 'before-action-ok])
    ))

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

(define (pc-action-on-after-resolve! action)
  (case (action-symbol action)
    ['traverse
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
     (describe-finish-traverse-action from to)
     (when (not (null? (location-items (action-target action))))
       (pick-up-items!))]

    ))


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
