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

; requirement is a lambda that's run on fragment's on-enter!
; on-resolve! is a lambda that's run when the decision is resolved
(serializable-struct
 decision
 (title
  description
  next-fragment
  requirement
  on-resolve!)
 #:constructor-name decision*)

(define
  (make-decision #:title title
                 #:description description
                 #:next-fragment next-fragment
                 #:requirement [requirement (λ () '())]
                 #:on-resolve![on-resolve! (λ () '())])
  
  (decision* title
             description
             next-fragment
             requirement
             on-resolve!))