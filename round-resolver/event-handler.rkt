#lang racket

(provide handle-interrupting-event!)

(require "../locations/locations.rkt"
         "../state/state.rkt"
         "../utils.rkt")

(require "event.rkt")

(define (handle-interrupting-event! event)
  (cond ((eq? (event-type event) 'spawn-enemies)
         (spawn-enemies (current-location))
         )
        (else
         (dev-note (string-append "handle-interrupting-event!: unknown event type: " (symbol->string (event-type event))))))
  '())