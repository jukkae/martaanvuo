#lang at-exp racket

(require
  "../../../1-index/content.rkt"

  "../../../3-types/action.rkt"
  "../../../3-types/choice.rkt"
  "../../../3-types/location.rkt"
  "../../../3-types/route.rkt"

  "../../../7-state/state.rkt"
  )

(provide make-traverse-choice)
(define (make-traverse-choice route direction)
  (define traverse-duration (route-traverse-time route))
  (make-choice
   'traverse
   (get-traverse-text route (current-location))
   (λ () (make-action
          #:symbol 'traverse
          #:actor (pc)
          #:duration traverse-duration
          #:target (location-id route)
          #:tags '(downtime)
          #:details (list direction)
          #:resolution-rules
          `(
            (set-route-traversed! (get-route-by-id ',(location-id route)))

            (define next-location-id
              ',(if (eq? direction 'a-to-b)
                    (route-b route)
                    (route-a route)))
            (define next-location (get-location-by-id next-location-id))
            (move-pc-to-location! next-location)
            'ok
            )
          #:on-before-rules
          `(
            (let/ec return
              (describe-begin-traverse-action (get-route-by-id ',(location-id route)) ',direction)
              (define next-location (get-route-by-id ',(location-id route)))
              (move-pc-to-location! next-location)

              (define elapsed-time 0)

              (when (not (location-has-detail? (current-location) 'no-encounters))
                (define encounter-roll (d 1 6))
                (define tn 3)
                (notice (format "Encounter roll: 1d6 < ~a: [~a] – ~a"
                                tn
                                encounter-roll
                                (if (< encounter-roll tn)
                                    "fail"
                                    "success")))
                (when (< encounter-roll tn)

                  (define resolve-events
                    (list
                     (make-event ,''spawn-enemies
                                 '() ; pack info about enemies / event here
                                 #:interrupting? #t)))
                  (define metadata '(interrupted))
                  (define duration
                    (exact-floor (/
                                  ,traverse-duration
                                  3)))

                  (set! elapsed-time duration)

                  (define world-tl (advance-time-until-next-interesting-event! duration #f))
                  (define world-events (timeline-events world-tl))

                  (define all-events (append world-events resolve-events))
                  (define all-metadata (append (timeline-metadata world-tl) metadata))

                  (define tl (timeline all-metadata all-events duration))

                  (process-timeline! tl)
                  (return tl))
                )

              'before-action-ok
              ))
          #:on-after-rules
          `(
            (describe-finish-traverse-action (get-route-by-id ',(location-id route)) ',direction)
            )

          ))))
