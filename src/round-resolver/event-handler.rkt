#lang racket

(provide handle-interrupting-event!)

(require 
  "event.rkt"

  "../core/utils.rkt"
  "../locations/locations.rkt")

(define (handle-interrupting-event! event)
  (cond ((eq? (event-type event) 'spawn-enemies)
         (spawn-enemies)
         )
        (else
         (dev-note (string-append "handle-interrupting-event!: unknown event type: " (symbol->string (event-type event))))))
  '())