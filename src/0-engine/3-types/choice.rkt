#lang at-exp typed/racket

(provide (all-defined-out))

(require
  "action.rkt")

; conceptually speaking, non-action-containing resolution-effects would have some overlap with fragments and decisions?
(define-struct choice
  ([symbol : Symbol]
   [name : String]
   [resolution-effect : (U action (->* () action))]))

