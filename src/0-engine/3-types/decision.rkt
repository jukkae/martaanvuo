#lang at-exp typed/racket

(provide (all-defined-out))

(require "../2-core/maybe.rkt")

; requirement is a lambda that's run on fragment's on-enter!
(struct decision
  ([title : String]
   [description : (Maybe String)]
   [next-fragment : (U Symbol (-> Symbol))]
   [time-taken : Natural]
   [requirement : (-> Boolean)])
  #:constructor-name decision*)

(define (make-decision #:title [title : String]
                        #:description [description : (Maybe String) '()]
                        #:next-fragment [next-fragment : (U Symbol (-> Symbol))]
                        #:time-taken [time-taken : Natural 0]
                        #:requirement [requirement : (-> Boolean) (λ () #t)]
                        )

  (decision* title
             description
             next-fragment
             time-taken
             requirement
             ))
