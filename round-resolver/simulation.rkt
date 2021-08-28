#lang racket

(provide advance-time-until-next-interesting-event!)

(require "../io.rkt"
         "../time.rkt"
         "../utils.rkt"
         "../world.rkt"
         
         "../state/state.rkt")

(require "event.rkt"
         "timeline.rkt")

; increment world time
; return a list of events that occur at new timestamp
(define (time++ [encounters? #f])
  (define new-elapsed-time (add1 (world-elapsed-time (current-world))))
  (set-world-elapsed-time!
   (current-world)
   new-elapsed-time)

  (get-daily-events-for-time new-elapsed-time)
  )

(define (get-daily-events-for-time time)
  (define events '())

  (define day (add1 (quotient time day-length)))
  (define time-today (remainder time day-length))
  
  (when (= (modulo time-today 100) 0)
    (dev-note (string-append "day " (number->string day) ", time " (number->string time-today)))
    (define suspend-action?
      (eq? (time-of-day-from-jiffies (world-elapsed-time (current-world)))
           'night))
    (define ev (make-event 'new-time-of-day (time-of-day-from-jiffies (world-elapsed-time (current-world))) #:interrupting? suspend-action?))
    (set! events (append-element events ev)))

  (when (= time-today 300)
    (define ev (make-event 'hunger-check '() #:interrupting? #f))
    (set! events (append-element events ev)))
  
  events)

; breaks on first action-suspending event
; and finishes after duration of jiffies,
; returns a timeline of events that occurred with metadata
(define (advance-time-until-next-interesting-event! jiffies [encounters? #f])
  (define metadata '())
  (define events '())
  (define counter 0)
  (let/ec break
    (for ([t jiffies])
      (set! counter (add1 counter))
      (define events-at-t (time++ encounters?))
      
      (set! events (append events events-at-t))

      ; If any of the events suspends action, then return early
      (define contains-action-suspending-event?
        (memf (λ (event) (event-interrupting? event)) events-at-t))

      ; early-exit
      (when contains-action-suspending-event?
        (set! metadata 'interrupted)
        (break))
      ))
  (timeline metadata events counter))