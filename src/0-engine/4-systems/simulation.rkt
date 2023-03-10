#lang at-exp racket

(provide advance-time-until-next-interesting-event!
         advance-time-by-iotas!)

(require racket/lazy-require)

(require "actors/actor.rkt"
         "actors/conditions.rkt"
         "events.rkt"
         "timelines.rkt"
         "items/item.rkt"
         "pc/pc.rkt"
         "world/time.rkt"

         "../2-core/core.rkt"
         "../2-core/input.rkt"
         "../2-core/output.rkt"

         "../3-types/actor.rkt"
         "../3-types/event.rkt"
         "../3-types/item.rkt"
         "../3-types/location.rkt"
         "../3-types/pc-actor.rkt"
         "../3-types/timeline.rkt"
         "../3-types/world.rkt"

         "../7-state/state.rkt")

(lazy-require ["pc/pc.rkt" (pc-hunger-level pc-fatigue-level)])
(lazy-require ["checks/checks.rkt" (check)])

; this should also likely have a mutator counterpart, to handle becoming less hungry
(define (hunger++)
  (define events '())

  (define old-pc-hunger-level (pc-hunger-level))
  (set-pc-actor-hunger! (current-pc) (+ (pc-actor-hunger (current-pc)) 1))
  (define new-pc-hunger-level (pc-hunger-level))

  (cond
    [(not (equal? old-pc-hunger-level new-pc-hunger-level))
     (notice (format "~a Otava is now ~a." (timestamp) new-pc-hunger-level))
     (make-event new-pc-hunger-level
                 (time-of-day-from-iotas (world-elapsed-time (current-world)))
                 #:interrupting? #f)]
    [else '()]))

(define (fatigue++)
  (define events '())

  (define old-pc-fatigue-level (pc-fatigue-level))
  (set-pc-actor-fatigue! (current-pc) (+ (pc-actor-fatigue (current-pc)) 1))
  (define new-pc-fatigue-level (pc-fatigue-level))

  (cond
    [(not (equal? old-pc-fatigue-level new-pc-fatigue-level))
     (notice (format "~a Otava is now ~a." (timestamp) new-pc-fatigue-level))
     (make-event new-pc-fatigue-level
                 (time-of-day-from-iotas (world-elapsed-time (current-world)))
                 #:interrupting? #f)]
    [else '()]))

; increment world time
; return a list of events that occur at new timestamp
(define (time++ [allow-interrupting-events? #f] [resting? #f])
  (define events '())

  (define new-world-elapsed-time (add1 (world-elapsed-time (current-world))))
  (set-world-elapsed-time! (current-world) new-world-elapsed-time)

  (current-elapsed-time (add1 (current-elapsed-time)))

  (define new-hunger-level? (hunger++))
  (when (not (null? new-hunger-level?))
    (set! events (append-element events new-hunger-level?)))

  (cond
    [(not resting?)
     (define new-fatigue-level? (fatigue++))
     (when (not (null? new-fatigue-level?))
       (set! events (append-element events new-fatigue-level?)))]
    [else
     (when (= (modulo (current-elapsed-time) 10) 0)
       (when (> (pc-actor-fatigue (pc)) 200)
         (set-pc-actor-fatigue! (pc) (- (pc-actor-fatigue (pc)) 5))))])

  (when (not (null? (current-location)))
    (for ([actor (location-actors (current-location))])
      (for ([condition (actor-conditions actor)])
        (actor-process-condition-tick actor condition))))

  (set! events (append events (get-daily-events-for-time new-world-elapsed-time)))

  (when allow-interrupting-events?
    (define encounter-roll (d 1 300))
    (cond
      [(<= encounter-roll 1)
       (notice (format "~a [~a] Random encounter!" (timestamp) "1d300 = 1"))
       (append-element! events (make-event 'spawn-encounter '() #:interrupting? #t))]))

  events)

(define (at-morning!)
  (when (flag-set? 'camp-set-up)
    (remove-flag 'camp-set-up))
  (define events '())

  (when (pc-has-condition-of-type? 'ankle-broken)
    (define tn 6) ; start higher, decrease with passing days
    (define roll-result (check "2d6" #:title "healing roll" #:target-number tn #:bonus '()))
    (match roll-result
      [(or 'critical-failure
            'serious-failure
            'failure)
        (p "Otava's ankle is still fucked.")
        ]
      [(or 'narrow-success
            'success
            'critical-success)
        (p "Otava's ankle is starting to feel better.")
        (actor-remove-condition-of-type! (pc) 'ankle-broken)
        (award-xp! 1)
        (wait-for-confirm)
        ]
    ))

  (when (not (null? (current-once-per-day-actions-done)))
    (define ev
      (make-event 'notice
                  (format "Once per day actions [~a] cleared." (current-once-per-day-actions-done))
                  #:interrupting? #f))
    (current-once-per-day-actions-done '())
    (set! events (append-element events ev)))

  (when (pc-has-item? 'salmon)
    (define item (pc-has-item? 'salmon))
    (set-item-id! item 'decaying-salmon)
    (set-item-name! item "decaying salmon")
    (define ev (make-event 'notice "The salmon's going bad." #:interrupting? #f))
    (set! events (append-element events ev)))

  (when (pc-has-item? 'decaying-berries)
    ; TODO: Collapsing & stacking items in general should be done in inventory itself
    (cond
      [(pc-has-item? 'moldy-berries)
       (define moldy-berries (pc-has-item? 'moldy-berries))
       (define decaying-berries (pc-has-item? 'decaying-berries))
       (set-item-quantity! moldy-berries
                           (+ (item-quantity moldy-berries) (item-quantity decaying-berries)))
       (remove-item! 'decaying-berries #:quantity-to-remove 'all)]
      [else
       (define item (pc-has-item? 'decaying-berries))
       (set-item-id! item 'moldy-berries)
       (set-item-name! item "Moldy berries")])
    (define ev (make-event 'notice "The berries now have a lot of mold on them." #:interrupting? #f))
    (set! events (append-element events ev)))
  (when (pc-has-item? 'berries)
    (define item (pc-has-item? 'berries))
    (set-item-id! item 'decaying-berries)
    (set-item-name! item "Decaying berries")
    (define ev (make-event 'notice "The berries have started going bad." #:interrupting? #f))
    (set! events (append-element events ev)))
  (when (pc-has-item? 'fresh-berries)
    (define item (pc-has-item? 'fresh-berries))
    (set-item-id! item 'berries)
    (set-item-name! item "Berries")
    (define ev
      (make-event 'notice
                  "The berries are not fresh anymore and will go bad after today."
                  #:interrupting? #f))
    (set! events (append-element events ev)))
  events)

(define (get-daily-events-for-time time)
  (define events '())

  (define day (add1 (quotient time day-length)))
  (define time-today (remainder time day-length))

  (when (and (= (modulo time-today 100) 0) (not (= 500 time-today)))
    (define new-time-of-day (time-of-day-from-iotas (world-elapsed-time (current-world))))
    (define interrupting?
      (cond
        [(equal? new-time-of-day 'night) #t]
        [(equal? new-time-of-day 'evening) #t]
        [else #f]))
    (notice (format "~a It is now ~a." (timestamp) new-time-of-day))

    (cond
      [(equal? new-time-of-day 'evening)
       (notice "Otava should think about how she'll spend the night.")])

    (define ev (make-event 'new-time-of-day new-time-of-day #:interrupting? interrupting?))
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
    (set! events (append events (time++ #f))))
  events
  (timeline metadata events iotas))

; breaks on first action-suspending event
; and finishes after duration of iotas,
; returns a timeline of events that occurred with metadata
(define (advance-time-until-next-interesting-event! iotas [encounters? #f] #:resting? [resting? #f])
  (define metadata '())
  (define events '())
  (define counter 0)
  (let/ec break
          (for ([t iotas])
            (set! counter (add1 counter))
            (define events-at-t (time++ encounters? resting?))

            (set! events (append events events-at-t))

            ; If any of the events suspends action, then return early
            (define contains-action-suspending-event?
              (memf (λ (event) (event-interrupting? event)) events-at-t))

            ; early-exit
            (when contains-action-suspending-event?
              (append-element! metadata 'interrupted)
              (break))))

  (timeline metadata events counter))
