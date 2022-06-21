#lang at-exp racket

(provide handle-interrupting-event!)

(require
  "../../2-core/core.rkt"

  "../../3-types/event.rkt"

  "../../4-systems/locations/locations.rkt"
  )

(define (handle-interrupting-event! event)
  (cond ((eq? (event-type event) 'spawn-encounter)
         (spawn-encounter))

        (else
         (dev-note (format "unknown event type ~a" (event-type event)))))
  '())
