#lang racket

(require "creatures.rkt")
(require "locations.rkt")

(define *forest* (new forest%))
(define *mountains* (new mountains%))
(define *river* (new river%))
(define *location* *forest*)

(define *creatures* '())

(define world%
  (class* object% ()
    (super-new)))

(define (spawn-enemy)
  (define r (random 2))
  (define enemy (cond ((= r 0) (new bloodleech%))
                      (else (new blindscraper%))))
  (set! *creatures* enemy))

(provide (all-defined-out))