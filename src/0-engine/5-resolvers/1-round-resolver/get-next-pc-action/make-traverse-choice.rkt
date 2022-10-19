#lang at-exp racket

(require
  "../../../1-index/content.rkt"

  "../../../3-types/action.rkt"
  "../../../3-types/choice.rkt"
  "../../../3-types/location.rkt"
  "../../../3-types/place.rkt"
  "../../../3-types/route.rkt"

  "../../../4-systems/actors/conditions.rkt"

  "../../../7-state/state.rkt"
  )

(provide make-traverse-choice)
(define (make-traverse-choice route direction)
  (define traverse-duration (route-traverse-time route))
  (when (actor-has-condition-of-type? (pc) 'ankle-broken)
     (set! traverse-duration (* 3 traverse-duration)))
  (make-choice
   'traverse
   (format "~a [~a ι]" (get-traverse-text route (current-location)) traverse-duration)
   (λ () (make-action
          #:symbol 'traverse
          #:actor (pc)
          #:duration traverse-duration
          #:target (location-id route)
          #:tags '(downtime)
          #:details (list direction)
          #:on-before-rules
          `(
            (let/ec return
              (describe-begin-traverse-action (get-route-by-id ',(location-id route)) ',direction)
              (define next-location (get-route-by-id ',(location-id route)))
              (move-pc-to-location! next-location)

              (define elapsed-time 0)

              (when (not (null? (location-encounter-types (current-location))))
                (define duration
                  (exact-floor (/
                                ,traverse-duration
                                3)))

                (set! elapsed-time duration)

                (define world-tl (advance-time-until-next-interesting-event! duration #f))
                (define world-events (timeline-events world-tl))

                (define bonus 0)
                (define bonus-str "+0")
                (define encounter-roll (+ (d 1 6) bonus))
                (define tn 5)
                (notice (format "~a Encounter roll: 1d6~a >= ~a: [~a] – ~a"
                                (timestamp)
                                bonus-str
                                tn
                                encounter-roll
                                (if (< encounter-roll tn)
                                    "fail"
                                    "success")))

                (when (< encounter-roll tn)
                  (define resolve-events
                    (list
                     (make-event ,''spawn-encounter
                                 '() ; pack info about enemies / event here
                                 #:interrupting? #t)))
                  (define metadata '(interrupted))

                  (define all-events (append world-events resolve-events))
                  (define all-metadata (append (timeline-metadata world-tl) metadata))

                  (define tl (timeline all-metadata all-events duration))

                  (process-timeline! tl)
                  (return tl))
                )

              'before-action-ok
              ))
          #:resolution-rules
          `(
            (set-route-traversed! (get-route-by-id ',(location-id route)))

            (define next-location-id
              ',(if (equal? direction 'a-to-b)
                    (route-b route)
                    (route-a route)))
            (define next-location (get-location-by-id next-location-id))
            (define tl (advance-time-until-next-interesting-event! ,traverse-duration #f))
            (process-timeline! tl)

            (cond
              [(timeline-interrupted? tl)
               (displayln "TIMELINE INTERRUPTED")
               'time-passing-handled
               ]
              [else
               (move-pc-to-location! next-location)
               'time-passing-handled
               ]
              )

            )
          #:on-after-rules
          `(
            (describe-finish-traverse-action (get-route-by-id ',(location-id route)) ',direction)
            )

          ))))
