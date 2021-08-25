#lang racket

(provide
 resolve-cancel-traverse-action!
 resolve-traverse-action!)

(require racket/lazy-require)

(require "../action.rkt"
         "../io.rkt"
         "../location.rkt"
         "../route.rkt"
         "../state/state.rkt"
         "../utils.rkt")

(require "../round-resolver/event.rkt"
         "../round-resolver/simulation.rkt"
         "../round-resolver/timeline.rkt")

(lazy-require
 ["../locations.rkt"
  (describe-begin-traverse-action
   describe-finish-traverse-action
   describe-cancel-traverse-action
   location-on-enter!
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
  (define elapsed-time 0)
  (cond ((not (pending? action))
         (describe-begin-traverse-action action)))

  (define result
    (let/ec return
      (move-pc-to-location! (action-target action))
      (when (not (pending? action))
        ; -> roll-for-encounter or something, more content than code -> belongs elsewhere

        (define encounter-roll
          (if (not (route-has-detail? (current-location) 'no-encounters))
              (d 1 6)
              #f))

        (when encounter-roll
          (define msg (string-append "Encounter roll: "
                                     (number->string encounter-roll)))
          (notice msg)
          (define encounter-event
            (if (< encounter-roll 4)
                (make-event 'spawn-enemies
                            '() ; pack info about enemies / event here
                            #:interrupting? #t)
                '()))

          (cond ((not (null? encounter-event))
                 ; create timeline to leverage existing code
                 (define events
                   (list
                    (make-event 'spawn-enemies
                                '() ; pack info about enemies / event here
                                #:interrupting? #t)))
                 (define metadata 'interrupted)
                 (define duration 1) ; half of traversal time - where to elapse the rest? after the event, likely
                 (define tl (timeline metadata events duration))

                 (set! elapsed-time (timeline-duration tl))

                 ; DUPLICATION, clean up
                 ; display events
                 #;(narrate-timeline tl)

                 ; look into https://docs.racket-lang.org/rebellion/Enum_Types.html for enums etc
                 (when (eq? (timeline-metadata tl) 'interrupted)
                   (return tl))))))
                          


      (set-route-traversed! (action-target action))

      (define next-location (if (memq 'a-to-b (action-details action))
                                (route-b (action-target action))
                                (route-a (action-target action))))
      (move-pc-to-location! next-location)


      (describe-finish-traverse-action action)
                          
      (when (not (null? (location-items (action-target action))))
        (pick-up-items!))

      'ok


      ))

  
  

  result
  )