#lang racket

(provide (all-defined-out))

(require racket/serialize)

(require "../io.rkt"
         "../utils.rkt"
         "../world.rkt"

         "../state/state.rkt")

(serializable-struct
 event
 (type
  details
  interrupting?
  at)
 #:constructor-name event*)

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
       ('evening (notice "It is now evening."))
       ('night (notice "It is now night."))
       ('morning (notice "It is now morning."))
       ))
    ; spawn-enemies is complicated to narrate outside of the event itself, so this is faster
    ('spawn-enemies '())
    ('not-hungry '()) ; this is usually not relevant
    (else
     (dev-note (format "narrate-event: unknown event type ~a" (event-type event))))))

(define
  (format-event-for-display event)
  (list
   (string-append " " (number->string (event-at event)) " ")
   (string-append " " (symbol->string (event-type event)) " ")
   (string-append " " (~s (event-details event)) " ")
   (string-append " "
                  (if (event-interrupting? event)
                      "yes"
                      "no")
                  " ")))
