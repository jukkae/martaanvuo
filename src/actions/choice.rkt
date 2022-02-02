#lang at-exp racket

(provide (all-defined-out))
(require racket/lazy-require)
(require "action.rkt")
(lazy-require
 ["../state/state.rkt"
  (pc
   )])

; conceptually speaking, non-action-containing resolution-effects would have some overlap with fragments and decisions?
(define-struct choice
  (symbol
   name
   resolution-effect)) ; resolution-effect is either a paramless lambda that produces an action, or an action

(define
  (make-pc-choice #:id id
                  #:text text
                  #:duration duration
                  #:target [target '()]
                  #:tags [tags '()]
                  #:details [details '()])
  (define action
    (make-action
     #:symbol id
     #:actor (pc)
     #:duration duration
     #:target target
     #:tags tags
     #:details '()))
  (choice
   id
   text
   action))
