#lang racket

(provide (all-defined-out))

(require racket/serialize)

(serializable-struct
 item
 ([name #:mutable]
  [details #:mutable]))