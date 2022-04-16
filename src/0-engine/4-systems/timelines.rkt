#lang at-exp racket

(provide (all-defined-out))

(require
  "events.rkt"

  "../2-core/io.rkt"
  "../2-core/core.rkt"

  "../3-types/event.rkt"
  "../3-types/timeline.rkt"

  "../../1-content/narration/narrate-timeline.rkt"
  )

(define (process-timeline! tl)
  (for ([event (timeline-events tl)])
    (case (event-type event)
      ['new-time-of-day
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
