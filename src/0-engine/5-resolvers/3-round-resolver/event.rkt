#lang at-exp racket

(provide (all-defined-out))

(require
  "../../2-core/io.rkt"
  "../../2-core/core.rkt"

  "../../3-types/event.rkt"
  "../../3-types/world.rkt"

  "../../7-state/state/state.rkt"
  )

(define (make-event
         type
         details
         #:interrupting? interrupting?)
  (event* type details interrupting? (world-elapsed-time (current-world))))
