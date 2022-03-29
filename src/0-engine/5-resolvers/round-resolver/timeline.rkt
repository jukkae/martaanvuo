#lang at-exp racket

(provide (all-defined-out))
(provide (all-from-out "0-types/timeline.rkt"))

(require "0-types/timeline.rkt"
         "event.rkt"

         "../../2-core/io.rkt"
         "../../2-core/core.rkt")

(define (narrate-timeline timeline)
  (define
    displayable-events
    (map
     format-event-for-display
     (timeline-events timeline)))
  #;(info-card
     (append
      (tbody (tr "at" "type" "details" "interrupts action?"))
      displayable-events)
     (format "Timeline, duration ~a" (timeline-duration timeline)))
  (for ([event (timeline-events timeline)])
    (narrate-event event)))

(define (process-timeline! tl)
  (for ([event (timeline-events tl)])
    (case (event-type event)
      ['new-time-of-day ; proc dailies here
       '()]
      ['not-hungry '()]
      ['hungry '()]
      ['very-hungry '()]
      ['starving '()]
      ['spawn-enemies '()]
      ['notice '()]
      [else
       (dev-note (format "process-timeline!: unknown event type ~a" (event-type event)))
       '()]))
  (narrate-timeline tl))
