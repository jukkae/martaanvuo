#lang at-exp racket

(provide (all-defined-out))

(require racket/serialize)

(serializable-struct
 world
 (places
  [routes #:mutable]
  day
  [elapsed-time #:mutable])
 #:transparent)
