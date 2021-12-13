#lang racket

(provide (all-defined-out))

(require racket/serialize)

(serializable-struct
 world
 (places
  day
  [elapsed-time #:mutable])
 #:transparent)
