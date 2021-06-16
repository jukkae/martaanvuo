#lang racket

(provide (all-defined-out))

(require racket/serialize)

(define (get-stance-range-numeric-value range)
  (case range
    ['engaged 0]
    ['close 1]
    ['mid 2]
    [else (error (string-append "get-stance-range-numeric-value: unknown range: "
                                (symbol->string range)))]))


(serializable-struct
 stance
 (enemy
  index
  range
  location))