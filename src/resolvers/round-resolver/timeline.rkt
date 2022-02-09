#lang at-exp racket

(provide (all-defined-out))

(require racket/serialize)
(require "event.rkt")

(serializable-struct
 timeline
 (metadata
  events
  duration))

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
