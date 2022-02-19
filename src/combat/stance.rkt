#lang at-exp typed/racket

(provide (all-defined-out))

(struct stance
  ([sign : String]
   [range : Symbol]
   [description : String])
  #:prefab
  #:mutable)

(define (get-stance-range-numeric-value range)
  (case range
    ['engaged 0]
    ['close 1]
    ['mid 2]
    [else (error (format "get-stance-range-numeric-value: unknown range: ~a" range))]))
