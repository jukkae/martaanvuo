#lang racket

(provide (all-defined-out))

(require racket/serialize)

(serializable-struct
 condition
 (type
  (details #:mutable)
  on-end-round! ; lambda
  ))