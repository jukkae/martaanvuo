#lang at-exp racket

(provide advance-time-until-next-interesting-event!
         advance-time-by-iotas!)

(require racket/lazy-require)

(require
  "events.rkt"
  "timelines.rkt"
  "items/item.rkt"
  "pc/pc.rkt"
  "world/time.rkt"

  "../2-core/core.rkt"

  "../3-types/event.rkt"
  "../3-types/item.rkt"
  "../3-types/pc-actor.rkt"
  "../3-types/timeline.rkt"
  "../3-types/world.rkt"

  "../7-state/state/state.rkt"
  )


(lazy-require ["pc/pc.rkt"
  (pc-hunger-level
   )])

; this should also likely have a mutator counterpart, to handle becoming less hungry
(define (hunger++)
  (define events '())

  (define old-pc-hunger-level (pc-hunger-level))
  (set-pc-actor-hunger! (current-pc) (+ (pc-actor-hunger (current-pc)) 1))
  (define new-pc-hunger-level (pc-hunger-level))

  (if (not (eq? old-pc-hunger-level new-pc-hunger-level))
      (make-event
       new-pc-hunger-level
       (time-of-day-from-iotas (world-elapsed-time (current-world)))
       #:interrupting? #f)

      '()))

; increment world time
; return a list of events that occur at new timestamp
(define (time++ [allow-interrupting-events? #f])
  (define events '())

  (define new-world-elapsed-time (add1 (world-elapsed-time (current-world))))
  (set-world-elapsed-time!
   (current-world)
   new-world-elapsed-time)

  (current-elapsed-time (add1 (current-elapsed-time)))

  (define new-hunger-level? (hunger++))
  (when (not (null? new-hunger-level?))
    (set! events (append-element events new-hunger-level?)))


  (set! events (append events (get-daily-events-for-time new-world-elapsed-time)))
  events)

(define (at-morning!)
  (define events '())
  (when (pc-has-item? 'decaying-berries)
    ; TODO: Collapsing & stacking items in general should be done in inventory itself
    (cond [(pc-has-item? 'moldy-berries)
           (define moldy-berries (pc-has-item? 'moldy-berries))
           (define decaying-berries (pc-has-item? 'decaying-berries))
           (set-item-quantity! moldy-berries (+ (item-quantity moldy-berries) (item-quantity decaying-berries)))
           (remove-item! 'decaying-berries #:quantity-to-remove 'all)]
          [else
           (define item (pc-has-item? 'decaying-berries))
           (set-item-id! item 'moldy-berries)
           (set-item-name! item "Moldy berries")])
    (define ev (make-event 'notice "The berries now have a lot of mold on them."  #:interrupting? #f))
    (set! events (append-element events ev))
    )
  (when (pc-has-item? 'berries)
    (define item (pc-has-item? 'berries))
    (set-item-id! item 'decaying-berries)
    (set-item-name! item "Decaying berries")
    (define ev (make-event 'notice "The berries have started going bad."  #:interrupting? #f))
    (set! events (append-element events ev))
    )
  (when (pc-has-item? 'fresh-berries)
    (define item (pc-has-item? 'fresh-berries))
    (set-item-id! item 'berries)
    (set-item-name! item "Berries")
    (define ev (make-event 'notice "The berries are not fresh anymore and will go bad after today."  #:interrupting? #f))
    (set! events (append-element events ev))
    )
  events
  )

(define (get-daily-events-for-time time)
  (define events '())

  (define day (add1 (quotient time day-length)))
  (define time-today (remainder time day-length))

  (when (= (modulo time-today 100) 0)
    (define ev (make-event 'new-time-of-day (time-of-day-from-iotas (world-elapsed-time (current-world))) #:interrupting? #f))
    (set! events (append-element events ev)))

  (when (= time-today 0)
    (set! events (append events (at-morning!))))

  events)

; Think if this should internally, conditionally use advance-time-until-next-interesting-event?
; (-> Natural (Listof Events)) ; s.b. diegetic time
(define (advance-time-by-iotas! iotas)
  (define metadata '())
  (define events '())
  (for ([t iotas])
    (set! events (append events (time++ #f)))
    )
  events
  (timeline metadata events iotas))

; breaks on first action-suspending event
; and finishes after duration of iotas,
; returns a timeline of events that occurred with metadata
(define (advance-time-until-next-interesting-event! iotas [encounters? #f])
  (define metadata '())
  (define events '())
  (define counter 0)
  (let/ec break
    (for ([t iotas])
      (set! counter (add1 counter))
      (define events-at-t (time++ encounters?))

      (set! events (append events events-at-t))

      ; If any of the events suspends action, then return early
      (define contains-action-suspending-event?
        (memf (λ (event) (event-interrupting? event)) events-at-t))

      ; early-exit
      (when contains-action-suspending-event?
        (set! metadata 'interrupted)
        (break))))

  (timeline metadata events counter))