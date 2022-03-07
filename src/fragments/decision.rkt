#lang at-exp typed/racket

(provide (all-defined-out))

(require "../core/maybe.rkt")

; requirement is a lambda that's run on fragment's on-enter!
; on-resolve! is a lambda that's run when the decision is resolved
(struct decision
  ([title : String]
   [description : (Maybe String)]
   [next-fragment : Symbol]
   [requirement : Procedure]
   [on-resolve! : Procedure])
  #:constructor-name decision*)

(define
  (make-decision #:title [title : String]
                 #:description [description : (Maybe String) '()]
                 #:next-fragment [next-fragment : Symbol]
                 #:requirement [requirement : Procedure (λ () '())]
                 #:on-resolve![on-resolve! : Procedure (λ () '())])

  (decision* title
             description
             next-fragment
             requirement
             on-resolve!))
