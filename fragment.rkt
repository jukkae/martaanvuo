#lang racket

(provide (all-defined-out))

(require racket/serialize)

(require "utils.rkt")

; store a list of closed paths / trees separately instead of storing that in the fragments themselves
(serializable-struct
 story-fragment
 (id
  description
  decisions
  on-enter!))