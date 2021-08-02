#lang racket

(provide (all-defined-out))

(require racket/serialize)

(serializable-struct
 route
 ([id #:mutable] ; mutable for s11n, but should not be mutated
  [a #:mutable]
  [b #:mutable]
  [details #:mutable]))

(define (get-traverse-text route)
  "get-traverse-text: TODO")