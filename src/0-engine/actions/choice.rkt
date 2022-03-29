#lang at-exp racket

(provide (all-defined-out))
(require racket/lazy-require)
(require "../3-types/types.rkt")
(lazy-require
 ["../state/state.rkt"
  (pc
   )])

; conceptually speaking, non-action-containing resolution-effects would have some overlap with fragments and decisions?
(define-struct choice
  (symbol
   name
   resolution-effect)) ; resolution-effect is either a paramless lambda that produces an action, or an action

