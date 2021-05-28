#lang racket

(provide (all-defined-out))

(require racket/serialize)

(define (get-stance-range-numeric-value range)
  (case range
    ['engaged 0]
    ['close 1]
    [else (error "get-stance-range-numeric-value: unknown range")]))


(serializable-struct
 stance
 (index
  range
  location))