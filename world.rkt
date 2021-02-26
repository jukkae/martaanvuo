#lang racket

(require "creatures.rkt")
(require "locations.rkt")
(require "narration.rkt")
(require "utils.rkt")
(require "pc.rkt")

(define *forest* (new forest%))
(define *mountains* (new mountains%))
(define *river* (new river%))
(define *location* *forest*)

(define *turn* 1)
(define *time-elapsed* 0)

(define *in-combat* #f)

(define *creatures* '())

(define *pc* (new pc%))

(define world%
  (class* object% ()
    (super-new)
    (define/public (set-combat in-combat)
      (set! *in-combat* in-combat))
    (define/public (advance-time . jiffies)
      (set! *time-elapsed* (add1 *time-elapsed*)))
    (define/public (advance-turn)
      (set! *turn* (add1 *turn*)))))

(define (reset-state)
  (set! *pc* (new pc%))
  (set! *turn* 1)
  (set! *time-elapsed* 0))

(define (spawn-enemy)
  (define r (random 2))
  (define enemy (cond ((= r 0) (new bloodleech%))
                      (else (new blindscraper%))))
  (set! *creatures* enemy))

(define (player-has-weapons?) (not (empty? (filter
                                            (lambda (item) (member 'stab (send item get-uses)))
                                            (get-field inventory *pc*)))))

(define (run-on-turn-actions . turn)
  (when *in-combat*
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

  (case *turn*
    [(3) (spawn-enemy)
         (set! *in-combat* true)
         (newline)
         (displayln (string-append (get-curse) " A " (send *creatures* get-name) " crawls forth. It looks at you like you would make a tasty meal for it."))
         (when (not (player-has-weapons?))
           (newline)
           (displayln (string-append "A weapon would be nice. But your hands are strong, and every living thing lives the same.")))]))


(provide (all-defined-out))