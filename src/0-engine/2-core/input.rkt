#lang at-exp racket

(provide (all-defined-out))

(require "output.rkt")

(define (wait-for-confirm)
  (case (get-output-state)
    ['dirty
     (newline)
     (prln "[Enter]")
     (newline)
     (define input (read-line))
     (set-output-state! 'clean)
     input]
    [else '()]))

(define (wait-for-input)
  (define input (string-trim (read-line)))
  (newline)
  (hr)
  (newline)
  input)
