#lang at-exp typed/racket

(provide (all-defined-out))

(require
  "action.rkt")

; conceptually speaking, non-action-containing resolution-effects would have some overlap with fragments and decisions?
(struct choice
  ([id : Symbol]
   [name : String]
   [resolution-effect : (U '()
                           action
                           (->* () (U '() action))
                           Sexp)]
   [available-in-combat? : Boolean]
   )
  #:mutable
  #:prefab
  #:constructor-name choice*)

(: make-choice (->* (Symbol String)
                    ((U '()
                        action
                        (->* () (U '() action))
                        Sexp)
                     #:available-in-combat? Boolean)
                    choice))
(define (make-choice
         id
         name
         [resolution-effect '()]
         #:available-in-combat? [available-in-combat? #f])
  (choice* id name resolution-effect available-in-combat?)
  )
