#lang at-exp racket

(provide (all-defined-out))

(require "narrate-event.rkt"

         "../../0-engine/2-core/core.rkt"
         "../../0-engine/2-core/io.rkt"

         "../../0-engine/3-types/event.rkt"
         "../../0-engine/3-types/timeline.rkt")

(define (narrate-timeline timeline)
  (define displayable-events (map format-event-for-display (timeline-events timeline)))
  #;(info-card (append (tbody (tr "at" "type" "details" "interrupts action?")) displayable-events)
               (format "Timeline, duration ~a" (timeline-duration timeline)))
  (for ([event (timeline-events timeline)])
    (narrate-event event)))
