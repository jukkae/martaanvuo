#lang at-exp racket

(provide (all-defined-out))

(require "events.rkt"

         "../2-core/io.rkt"
         "../2-core/core.rkt"

         "../3-types/event.rkt"
         "../3-types/timeline.rkt"

         "../../1-content/narration/narrate-timeline.rkt")

(define (timeline-interrupted? tl)
  (findf (λ (event) (event-interrupting? event)) (timeline-events tl)))

(define (process-timeline! tl)
  (for ([event (timeline-events tl)])
    (case (event-type event)
      ['new-time-of-day '()
       ]
      ['not-hungry '()]
      ['hungry '()]
      ['very-hungry '()]
      ['starving '()]
      ['tired '()]
      ['drained '()]
      ['exhausted '()]
      ['spawn-encounter '()]
      ['notice '()]
      [else
       (dev-note (format "process-timeline!: unknown event type ~a" (event-type event)))
       '()]))
  (narrate-timeline tl))
