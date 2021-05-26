#lang racket

(provide (all-defined-out))

(require "io.rkt")
(require "fragment.rkt")
(require "situation.rkt")

; engine: round-resolver -> fragment-handler or something
(define (current-fragment-on-begin-round!)
  (paragraph (story-fragment-description (situation-current-fragment *situation*)))
  )