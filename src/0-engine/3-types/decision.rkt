#lang at-exp typed/racket

(provide (all-defined-out))

(require "../2-core/maybe.rkt")

; requirement is a lambda that's run on fragment's on-enter!
; on-resolve! is a lambda that's run when the decision is resolved
(struct decision
  ([title : String]
   [description : (Maybe String)]
   [next-fragment : (U Symbol (-> Symbol))] ; TODO: this vs. on-resolve!
   [time-taken : Natural]
   [requirement : (-> Boolean)]
   [on-resolve! : (-> Null)])
  #:constructor-name decision*)

(define (make-decision #:title [title : String]
                        #:description [description : (Maybe String) '()]
                        #:next-fragment [next-fragment : (U Symbol (-> Symbol))]
                        #:time-taken [time-taken : Natural 0]
                        #:requirement [requirement : (-> Boolean) (λ () #t)]
                        #:on-resolve![on-resolve! : (-> Null) (λ () '())])

  (decision* title
             description
             next-fragment
             time-taken
             requirement
             on-resolve!))
