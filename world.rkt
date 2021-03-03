#lang racket

(require "actions.rkt")
(require "actors.rkt")
(require "creatures.rkt")
(require "items.rkt")
(require "locations.rkt")
(require "narration.rkt")
(require "utils.rkt")
(require "pc.rkt")

(define *pc* (new pc%))

(define world%
  (class* object% ()
    (field [turn 0]) ; meta
    (field [in-combat #f]) ; situational
    (field [elapsed-time 0]) ; jiffies
    (field [time-of-day 'midday])
    (field [locations (make-hash)])
    (field [current-location '()]) ; should be defined to (send pc get-current-location)

    (super-new)
    (define/public (make-connections)
      (begin
        (for ([i (in-range 0 10)])
          (define location (make-location #:index i))
          (hash-set! locations i location))

        (for ([i (in-range 0 10)])
          (define location (hash-ref locations i))
          (define n (random 1 4))

          (for ([j (in-range 0 n)])
            (define next-neighbor-index (random 0 10))
            (define neighbor (hash-ref locations next-neighbor-index))
            (set-field! neighbors
                        location
                        (cons neighbor (get-field neighbors
                                                  location)))))
        (set-field! current-location
                    this
                    (hash-ref locations 0))))))

(define (make-new-world)
  (define world (new world%))
  (send world make-connections)
  world)

(define (advance-time! world jiffies)
  (define new-elapsed-time (+ (get-field elapsed-time world)
                              jiffies))
  (set-field! elapsed-time
              world
              new-elapsed-time))

(define *action-queue* '())

; update status effects etc
(define (begin-turn! world)
  (set-field! turn
              world
              (add1 (get-field turn world)))
  (displayln
   (string-append
    "-- Turn " (number->string (get-field turn world))
    ", "
    "elapsed time: " (number->string (get-field elapsed-time world)) " jiffies"
    ", "
    "location: " (number->string (get-field index (get-field current-location world)))
    ", "
    "time of day: " (symbol->string (get-field time-of-day world))
    ))
  (newline))

(define (on-turn! world)
  (cond ((= (modulo (get-field turn world) 3) 0) (spawn-enemy world))))

(define (resolve-actions! world actions)
  #;(displayln "-- *world* : resolve-actions!") '())

(define (end-turn! world)
  #;(displayln (append-string "-- *world* : end-turn!")) '())

(define (reset-state world)
  (set! world (make-new-world))
  (set! *pc* (new pc%)))

(define (spawn-enemy world)
  (define location (get-field current-location world))
  (define r (random 2))
  (define enemy (cond ((= r 0) (new bloodleech%))
                      (else (new blindscraper%))))
  (send location add-actor! enemy))

(define (player-has-weapons?) (not (empty? (filter
                                            (lambda (item) (member 'stab (send item get-uses)))
                                            (get-field inventory *pc*)))))



(define (describe-situation world)
  (displayln (send (get-field current-location world) get-description))
  (newline))


(define (get-world-actions world actor)
  (define location-actions (send (get-field current-location world)
                                 get-interactions))
  (define next-location-choices (send (get-field current-location world)
                                      get-visible-neighbors))
  (define generic-actions (send actor get-generic-actions world))
  (define all-actions (append location-actions next-location-choices generic-actions))
  all-actions)

(define (resolve-action! world action actor)
  (case (action-symbol action)
    ['search (begin
               (define loot (send (get-field current-location world) search))
               (cond ((eq? loot 'nothing) (displayln "You find nothing of interest."))
                     (else
                      (newline)
                      (displayln (string-append "Ah ha! You find " (send loot get-inline-description) " half buried under a rock. A gift."))
                      (newline)
                      (displayln (string-append "You pick up the " (send loot get-short-description) "."))
                      (set-field! inventory *pc* (cons loot (get-field inventory *pc*)))
                      (when (is-a? loot sapling-finger%) #;(win) (error "world.rkt: update-state!: Reimplement win!"))))
               (newline)
               (advance-time! world (action-duration action)))]
    ['inventory (print-inventory (get-list-inline-description (get-field inventory *pc*)))]
    ['go-to-neighboring-location (begin
                                   (newline) ; dunno where to put this
                                   (send (get-field current-location world) on-exit!)
                                   
                                   (advance-time! world  (action-duration action))
                                   
                                   (set-field! current-location world (action-target action))
                                   (send (get-field current-location world) on-enter!))]
    
    [else (error (string-append "Unknown action: " (symbol->string (action-symbol action))))]))

(define (add-action-to-queue *world* action actor)
  (displayln "FIND ME TOO AND FIX ME TOO"))

(provide (all-defined-out))