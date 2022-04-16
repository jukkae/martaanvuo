#lang at-exp racket

(provide (all-defined-out))

(require
  "../../0-engine/3-types/event.rkt"
  "../../0-engine/2-core/core.rkt"
  "../../0-engine/2-core/io.rkt"
  )

(define (narrate-event event)
  (case (event-type event)
    ['new-time-of-day
     (define event-text
       (format "It is now ~a." (event-details event))
     )
     (format "~a ι: ~a" (event-at event) event-text)
     ]

    ; spawn-enemies is complicated to narrate outside of the event itself, so this is faster
    ['spawn-enemies '()]
    ['not-hungry '()] ; this is usually not relevant
    ['hungry (notice (format "~a ι: ~a" (event-at event) "Otava is now hungry."))]
    ['very-hungry (notice (format "~a ι: ~a" (event-at event) "Otava is now very hungry."))]
    ['starving (notice (format "~a ι: ~a" (event-at event) "Otava is now starving."))]
    ['notice (notice (format "~a ι: ~a" (event-at event) (event-details event)))]
    [else
     (dev-note (format "narrate-event: unknown event type ~a" (event-type event)))]))
