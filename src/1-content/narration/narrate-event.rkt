#lang at-exp racket

(provide (all-defined-out))

(require
  "../../0-engine/3-types/event.rkt"
  "../../0-engine/2-core/core.rkt"
  "../../0-engine/2-core/io.rkt"
  )

(define (narrate-event event)
  (case (event-type event)
    ('new-time-of-day
     (case (event-details event)
       ('afternoon (notice "It is now afternoon."))
       ('midday (notice "It is now midday."))
       ('evening (notice "It is now evening."))
       ('night (notice "It is now night."))
       ('morning (notice "It is now morning."))))

    ; spawn-enemies is complicated to narrate outside of the event itself, so this is faster
    ('spawn-enemies '())
    ('not-hungry '()) ; this is usually not relevant
    ('hungry (notice "Otava is now hungry."))
    ('very-hungry (notice "Otava is now very hungry."))
    ('starving (notice "Otava is now starving."))
    ['notice (notice (event-details event))]
    (else
     (dev-note (format "narrate-event: unknown event type ~a" (event-type event))))))
