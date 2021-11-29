#lang racket

(provide (all-defined-out))

(require racket/serialize)

(serializable-struct
 stance
 (sign
  range
  description))

(define (get-stance-range-numeric-value range)
  (case range
    ['engaged 0]
    ['close 1]
    ['mid 2]
    [else (error (format "get-stance-range-numeric-value: unknown range: ~a" range))]))
