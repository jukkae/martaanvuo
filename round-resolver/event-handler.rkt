#lang racket

(provide handle-interrupting-event!)

(require "../locations/locations.rkt"
         "../state/state.rkt")

(require "event.rkt")

(define (handle-interrupting-event! event)
  (cond ((eq? (event-type event) 'spawn-enemies)
         (spawn-enemies (current-location))
         )
        (else
         (displayln "handle-interrupting-event!: unknown event type")))
  '())