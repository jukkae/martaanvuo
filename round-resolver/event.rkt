#lang racket

(provide (all-defined-out))

(require racket/serialize)

(require "../io.rkt"
         "../situation.rkt"
         "../world.rkt")

(serializable-struct
 event
 (type
  details
  interrupting?
  at)
 #:constructor-name event*)

; type used in engine / round-resolver
(define (make-event
         type
         details
         interrupting?)
  (event* type details interrupting? (world-elapsed-time (situation-world *situation*))))

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
    (else (displayln (string-append "narrate-event: unknown event type "
                                    (symbol->string (event-type event)))))))