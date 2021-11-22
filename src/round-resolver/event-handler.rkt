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
         (dev-note (format "unknown event type ~a" (event-type event)))))
  '())