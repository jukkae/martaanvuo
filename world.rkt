#lang racket

(require "actions.rkt")
(require "actors.rkt")
(require "creatures.rkt")
(require "items.rkt")
(require "locations.rkt")
(require "narration.rkt")
(require "utils.rkt")
(require "pc.rkt")

(define world%
  (class* object% ()
    (field [turn 0]) ; meta
    (field [in-combat #f]) ; situational
    (field [elapsed-time 0]) ; jiffies
    (field [time-of-day 'midday])
    (field [locations (make-hash)])
    (field [current-location '()]) ; should be defined to (send pc get-current-location)

    (field [pc (new pc%)])

    (field [action-queue '()])

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
                    (hash-ref locations 0))))
    
    (define/public (get-current-enemies)
      (define all-actors (get-field actors current-location))
      (set! all-actors (remove pc all-actors))
      all-actors)))

(define (make-new-world)
  (define world (new world%))
  (send world make-connections)
  (define location (get-field current-location world))
  (send location add-actor! (get-field pc world))
  world)

(define (advance-time! world jiffies)
  (define new-elapsed-time (+ (get-field elapsed-time world)
                              jiffies))
  (set-field! elapsed-time
              world
              new-elapsed-time))

(define (begin-turn! world)
  (set-field! turn
              world
              (add1 (get-field turn world)))

  (newline)
  (displayln
   (string-append
    "-- Turn " (number->string (get-field turn world))
    ", "
    "elapsed time: " (number->string (get-field elapsed-time world)) " jiffies"
    ", "
    "location: " (number->string (get-field index (get-field current-location world)))
    ", "
    "time of day: " (symbol->string (get-field time-of-day world))
    ", "
    "in combat: " (if (get-field in-combat world)
                      "yes"
                      "no")
    ))
  (newline))

(define (on-turn! world)
  (cond ((and
          (= (modulo (get-field turn world) 3) 0)
          (not (get-field in-combat world)))
         (begin (spawn-enemies world 2)))))

(define (resolve-actions! world)
  (map (λ (action)
         (displayln "Resolving action:")
         (displayln action))
       ; (resolve-action! world action)) ; TODO this needs to get access to actor somehow – store in action when creating it?
       (get-field action-queue world)))

(define (end-turn! world)
  #;(displayln (append-string "-- *world* : end-turn!")) '())

(define (spawn-enemies world number)
  (displayln "Blight! Enemies appear!")
  (newline)
  (for ([i (in-range 0 number)])
    
    (define location (get-field current-location world))
    (define r (random 1))
    (define enemy (cond ((= r 0) (new bloodleech%))
                        (else (new blindscraper%))))
    (send location add-actor! enemy))
  (set-field! in-combat world #t))

(define (player-has-weapons? pc)
  (not (empty? (filter
                (lambda (item) (member 'stab (send item get-uses)))
                (get-field inventory pc)))))



(define (describe-situation world)
  (displayln (send (get-field current-location world) get-description))
  (newline))


(define (get-world-actions world actor)
  (define location-actions
    (if (not (get-field in-combat world))
        (send (get-field current-location world)
              get-interactions)
        '()))
  (define next-location-choices
    (if (not (get-field in-combat world))
        (send (get-field current-location world)
              get-visible-neighbors)
        '()))

  (define combat-actions (send actor get-combat-actions world))
  (define generic-actions (send actor get-generic-actions world))
  (define all-actions (append location-actions next-location-choices combat-actions generic-actions))
  all-actions)

(define (resolve-player-action! world action actor)
  (case (action-symbol action)
    ['search
     (begin
       (define loot (send (get-field current-location world) search))
       (cond ((eq? loot 'nothing) (displayln "You find nothing of interest."))
             (else
              (newline)
              (displayln (string-append "Ah ha! You find " (send loot get-inline-description) " half buried under a rock. A gift."))
              (newline)
              (displayln (string-append "You pick up the " (send loot get-short-description) "."))
              (set-field! inventory actor (cons loot (get-field inventory actor)))
              (when (is-a? loot sapling-finger%) #;(win) (error "world.rkt: update-state!: Reimplement win!"))))
       (newline)
       (advance-time! world (action-duration action)))]
    ['inventory
     (print-inventory
      (get-list-inline-description
       (get-field inventory actor)))]
    ['go-to-neighboring-location
     (begin
       (newline) ; dunno where to put this
       (send (get-field current-location world) remove-actor! actor) ; hacky
       (send (get-field current-location world) on-exit!)

                                   
       (advance-time! world  (action-duration action))

       (set-field! current-location world (action-target action))
       (send (get-field current-location world) add-actor! actor) ; ungh
       (send (get-field current-location world) on-enter!))]
    
    [else (error (string-append "Unknown player action: " (symbol->string (action-symbol action))))]))

(define (resolve-enemy-action! world action actor)
  (case (action-symbol action)
    ['attack (begin
               (displayln "hojo! enemy attack action!")
               (newline))]
    [else (error (string-append "Unknown enemy action: " (symbol->string (action-symbol action))))]))

(define (resolve-action! world action actor)
  (if (is-a? actor pc%)
      (resolve-player-action! world action actor)
      (resolve-enemy-action! world action actor))
  (set-field!
   action-queue
   world
   (if (pair? (get-field action-queue world))
       (cdr (get-field action-queue world)) ; pop stack's topmost element
       '())))

(define (add-action-to-queue world action actor)
  (define new-actions
    (append (get-field action-queue world)
            (list action)))
  (set-field! action-queue world new-actions))

(provide (all-defined-out))