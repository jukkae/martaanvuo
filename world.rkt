#lang racket

(require "actions.rkt")
(require "actors.rkt")
(require "creatures.rkt")
(require "items.rkt")
(require "locations.rkt")
(require "narration.rkt")
(require "utils.rkt")
(require "pc.rkt")

(define *forest* (new forest%))
(define *mountains* (new mountains%))
(define *river* (new river%))
(define *location* *forest*)

(define *creatures* '())

(define *pc* (new pc%))

(define *world* (make-hash))
(hash-set! *world* 'turn 0)
(hash-set! *world* 'in-combat #f)
(hash-set! *world* 'elapsed-time 0)

(define (make-new-world)
  (define world (make-hash))
  (hash-set! world 'turn 0)
  (hash-set! world 'in-combat #f)
  (hash-set! world 'elapsed-time 0)
  world)

(define (advance-time! jiffies)
  (hash-set! *world* 'elapsed-time (+ (hash-ref *world* 'elapsed-time) jiffies)))

(define *action-queue* '())

; do things like update status effects etc
(define (begin-turn! world)
  (hash-set! *world* 'turn (add1 (hash-ref *world* 'turn))) ; bump turn
  (displayln (append-string "-- *world* : begin-turn!, turn " (number->string (hash-ref *world* 'turn)))))

(define (resolve-actions! world actions)
  (displayln "-- *world* : resolve-actions!"))

(define (end-turn! world)
  (displayln (append-string "-- *world* : end-turn!")))

(define (reset-state)
  (set! *world* (make-new-world))
  (set! *pc* (new pc%)))

(define (spawn-enemy)
  (define r (random 2))
  (define enemy (cond ((= r 0) (new bloodleech%))
                      (else (new blindscraper%))))
  (set! *creatures* enemy))

(define (player-has-weapons?) (not (empty? (filter
                                            (lambda (item) (member 'stab (send item get-uses)))
                                            (get-field inventory *pc*)))))

(define (describe-situation world)
  (newline)
  (displayln (string-append "-- Turn " (number->string (hash-ref world 'turn)) ", elapsed time: " (number->string (hash-ref world 'turn)) " jiffies"))
  (newline)
  #;(when (not (hash-ref world 'in-combat) (displayln (send *location* get-description))))
  #;(when (and (not *in-combat*) (= *turn* 2))
    (begin
      (newline)
      (displayln (send *pc* get-a-hunch))))
  #;(when *in-combat* (displayln (string-append "You are grappling with a " (send *creatures* get-name) ". [" (number->string (get-field hp *creatures*)) " HP]"))))

#;(define (run-on-turn-actions . turn)
  #;(when *in-combat*
    (newline)
    (displayln (string-append "The " (send *creatures* get-name) " attacks you."))
    (define to-hit (+ (d 2 6) 1))
    (define target 6)
    (define damage (d 1 2))
    (displayln (string-append "[to hit: 2d6+1: " (number->string to-hit) "]"))
    (if (> to-hit target)
        (begin (displayln (string-append "[dmg: 1d2: " (number->string damage) "]"))
               (displayln "Oof. That hurt.")
               (send *pc* hit damage)
               (if (<= (get-field hp *pc*) 0)
                   (begin (displayln "You are dead.")
                          (error "run-on-turn-actions: Implement dying")
                          'u-ded) ; TODO
                   (displayln (string-append "You have " (number->string (get-field hp *pc*)) "HP."))))
        (begin (displayln "You dodge."))))

  #;(case *turn*
    [(3) (spawn-enemy)
         (set! *in-combat* true)
         (newline)
         (displayln (string-append (get-curse) " A " (send *creatures* get-name) " crawls forth. It looks at you like you would make a tasty meal for it."))
         (when (not (player-has-weapons?))
           (newline)
           (displayln (string-append "A weapon would be nice. But your hands are strong, and every living thing lives the same.")))]))

(define (update-state! action)
  '()
  #;(case (action-symbol action)
    ['inventory (print-inventory (get-list-inline-description (get-field inventory *pc*)))]
    ['go-on (begin (newline)
                   (displayln (take-random '("Better get to it, then." "You keep on walking.")))
                   (send *world* advance-time))]
    ['stab (begin (fight)
                  (send *world* advance-time))]
    ['brawl (begin (brawl)
                   (send *world* advance-time))]
    ['camp (displayln (take-random '("You are not tired." "You are barely getting started." "It is too early to camp.")))]
    ['run (newline) (displayln (take-random '("You try to run.")))]
    ['go-to-mountains (error "implement go to-action")]
    ['go-to-river (error "implement go to-action")]
    ['go-downriver (error "implement go to-action")]
    [else (error (string-append "Unknown action: " (symbol->string (action-symbol action))))]))


; TODO: These should be PC actions
(define (fight)
  (send *world* set-combat #t)
  (define to-hit (+ (d 2 6) (get-field attack-skill *pc*)))
  (define target (get-field defense *creatures*))
  (define damage (d 1 4))
  (newline)
  (displayln (string-append (get-narration-for-stab) " [2d6+1: " (number->string to-hit) "]"))
  (cond ((>= to-hit target)
         (displayln (get-narration-for-successful-stab))
         (displayln (string-append "[damage: " (number->string damage) " HP]"))
         (define result (send *creatures* hit damage))
         (when (equal? result 'dead) (begin (displayln (string-append "The " (send *creatures* get-name) " is dead."))
                                            (send *world* set-combat #f))))
        (else (displayln (string-append (get-curse) " You miss.")))))

(define (brawl)
  (send *world* set-combat #t)
  (define to-hit (+ (d 2 6) (get-field attack-skill *pc*) 2)) ; +2 to hit bonus; having +defense against this opponent would be great
  (define target (get-field defense *creatures*))
  (define damage (d 1 2))
  (newline)
  (displayln (string-append (get-narration-for-brawl) " [2d6+1: " (number->string to-hit) "]"))
  (cond ((>= to-hit target)
         (displayln (get-narration-for-successful-brawl))
         (displayln (string-append "[damage: " (number->string damage) " HP]"))
         (define result (send *creatures* hit damage))
         (when (equal? result 'dead) (begin (displayln (string-append "The " (send *creatures* get-name) " is dead."))
                                            (send *world* set-combat #f))))
        (else (displayln (string-append (get-curse) " You can't get a good hold of the enemy.")))))



(define (get-location-actions)
  (define actions (send *location* get-interactions))
  actions)

(define (get-world-actions world actor)
  (define location-actions (send *location* get-interactions))
  (define next-location-choices (send *location* get-visible-exits))
  (define generic-actions (send actor get-generic-actions world))
  (define all-actions (append location-actions next-location-choices generic-actions))
  all-actions)

(define (resolve-action world action actor)
  (case (action-symbol action)
    ['search (begin
               (define loot (send *location* search))
               (cond ((eq? loot 'nothing) (displayln "You find nothing of interest."))
                     (else
                      (newline)
                      (displayln (string-append "Ah ha! You find " (send loot get-inline-description) " half buried under a rock. A gift."))
                      (newline)
                      (displayln (string-append "You pick up the " (send loot get-short-description) "."))
                      (set-field! inventory *pc* (cons loot (get-field inventory *pc*)))
                      (when (is-a? loot figurine%) #;(win) (error "world.rkt: update-state!: Reimplement win!"))))
               (newline)
               (advance-time! (action-duration action)))]
    [else (error (string-append "Unknown action: " (symbol->string (action-symbol action))))]))

(define (add-action-to-queue *world* action actor)
  (displayln "FIND ME TOO AND FIX ME TOO"))

(provide (all-defined-out))