#lang at-exp racket

(provide (all-defined-out))
(provide (all-from-out "0-types/event.rkt"))

(require
  "0-types/event.rkt"

  "../../2-core/io.rkt"
  "../../2-core/core.rkt"

  "../../3-types/world.rkt"

  "../../7-state/state/state.rkt"
  )

(define (make-event
         type
         details
         #:interrupting? interrupting?)
  (event* type details interrupting? (world-elapsed-time (current-world))))

; narration content to event,
; function to call narration in engine / round-resolver
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

(define
  (format-event-for-display event)
  (list
   (number->string (event-at event))
   (symbol->string (event-type event))
   (~s (event-details event))
   (if (event-interrupting? event)
       "yes"
       "no")))
