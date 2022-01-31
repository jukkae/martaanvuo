#lang racket

(provide resolve-action!)

(require racket/lazy-require)

(require
  "downtime-actions.rkt"
  "special-actions.rkt"
  "traverse-action.rkt"

  "../round-resolver/event.rkt"
  "../round-resolver/simulation.rkt"
  "../round-resolver/timeline.rkt"

  "../../actions/action.rkt"

  "../../actors/actor.rkt"
  "../../actors/pc-actor.rkt"

  "../../combat/combat-action-resolver.rkt"

  "../../core/io.rkt"
  "../../core/utils.rkt"

  "../../enemies/blindscraper-actions.rkt"
  "../../enemies/grabberkin-actions.rkt"

  "../../items/item.rkt"

  "../../locations/0-types/location.rkt"

  "../../pc/pc.rkt"

  "../../state/state.rkt"

  "../../world/time.rkt"
  "../../world/world.rkt"

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
      (set! result (dispatch-to-sub-resolver! action))
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
       (cond ((not (pending? action))
              (describe-begin-traverse-action action))
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
     (describe-finish-traverse-action action)
     (when (not (null? (location-items (action-target action))))
       (pick-up-items!))]

    ))

(define (time-until-next-morning)
  (let* ([time (world-elapsed-time (current-world))]
         [time-today (remainder time day-length)])
    (- day-length time-today)))

(define (time-until-next-time-of-day)
  (- 100 (remainder (world-elapsed-time (current-world)) 100)))

(define (progress-until-next-day! action)
  (set-action-duration! action (time-until-next-morning))
  'ok)

(define (progress-until-next-time-of-day! action)
  (set-action-duration! action (time-until-next-time-of-day))
  'ok)

(define (dispatch-to-sub-resolver! action)
  (case (action-symbol action)
    ; "special" actions first
    ['end-run (resolve-special-action! action)]
    ['back-off (resolve-special-action! action)]
    ['win-game (resolve-special-action! action)]
    ['skip (resolve-special-action! action)]
    ['go-to-location (resolve-go-to-action! action)]
    ['traverse (resolve-traverse-action! action)]
    ['cancel-traverse (resolve-cancel-traverse-action! action)]

    ; placeholder / WIP
    ['camp 'ok]
    #;['sleep (resolve-sleep-action! action)]
    ['sleep (progress-until-next-day! action)]
    ['rest (progress-until-next-time-of-day! action)]
    ['eat
     (define food-item (action-target action))
     (displayln (format "TARGET: ~a" food-item))
     (define food-tier
       (case (item-id food-item)
        ['fresh-berries 0]
        ['food-ration 1]
        ['vatruska 2]
        [else (dev-note (format "Unknown comestible ~a" (item-id food-item)))
              1])
      )
     (decrease-pc-hunger-level food-tier)

     (case (item-id food-item)
      ['fresh-berries (p "The berries are invigoratingly sweet.")]
      ['food-ration (p "The ration's dry and bland, but filling.")]
      ['vatruska (p "The vatruska tastes heavenly.")])
     (remove-item! (item-id food-item))]

    ; the rest
    ['melee (resolve-melee-action! action)]
    ['shoot (resolve-shoot-action! action)]
    ['forage (resolve-forage-action! action)]
    ['flee (resolve-flee-action! action)]
    ['break-free (resolve-break-free-action! action)]

    ['anklebreaker (resolve-anklebreaker-action! action)]
    ['pull-under (resolve-pull-under-action! action)]
    ['release-grip 'grip-released]

    ['go-to-engaged (resolve-go-to-engaged-action! action)]
    ['go-to-close (resolve-go-to-close-action! action)]


    ['inflict-status (resolve-inflict-status-action! action)]

    ['modify-status (resolve-modify-status-action! action)]

    ['inflict-condition (resolve-inflict-condition-action! action)]

    [else
     (dev-note (format "resolve-action!: unknown action type ~a" (action-symbol action)))
     ]))


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