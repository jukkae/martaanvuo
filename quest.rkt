#lang racket

(provide (all-defined-out))

(require racket/serialize)

(serializable-struct
 quest
 (id
  [title #:mutable]
  [status #:mutable]
  [notes #:mutable]))