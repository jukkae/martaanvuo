#lang at-exp racket

(provide (all-defined-out))

(require "../1-index/state.rkt"

         "../2-core/core.rkt")

(define (go-to-fragment id)
  (current-fragment-id id))
