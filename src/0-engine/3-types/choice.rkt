#lang at-exp typed/racket

(provide (all-defined-out))

(require
  "action.rkt")

; conceptually speaking, non-action-containing resolution-effects would have some overlap with fragments and decisions?
; TODO: Shaman questline, and similar, would benefit from Place-choices having conditions, similar to decisions!
; (If conditions empty, then availability according to other modifications)
(struct choice
  ([id : Symbol]
   [name : String]
   [resolution-effect : (U '()
                           action
                           Symbol
                           (->* () (U '() action))
                           Sexp)]
   [available-in-combat? : Boolean]
   [unavailable? : Boolean]
   )
  #:mutable
  #:prefab
  #:constructor-name choice*)

(: make-choice (->* (Symbol String)
                    ((U '()
                        action
                        Symbol ; things like 'recurse
                        (->* () (U '() action))
                        Sexp)
                     #:available-in-combat? Boolean
                     #:unavailable? Boolean)
                    choice))
(define (make-choice
         id
         name
         [resolution-effect '()]
         #:available-in-combat? [available-in-combat? #f]
         #:unavailable? [unavailable? #f])
  (choice* id name resolution-effect available-in-combat? unavailable?)
  )

(define (make-unavailable-choice name reason)
  (make-choice
   'unavailable
   (format "(~a) – ~a" name reason)
   #:unavailable? #t
   ))
