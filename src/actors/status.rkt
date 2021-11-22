#lang racket

(provide (all-defined-out))

(require racket/serialize)

(serializable-struct
 status
 (type
  (lifetime #:mutable))
 #:transparent)
