#lang racket

(provide (all-defined-out))

(require "utils.rkt")

(define current-session-score-dice (make-parameter 1))
(define (current-session-score-dice++)
  (current-session-score-dice (add1 (current-session-score-dice))))

(define session-flags (make-parameter '()))

(define (set-session-flag flag)
  (when (not (session-flag-set? flag))
    (session-flags (append-element (session-flags) flag))))

(define (remove-session-flag flag)
  (when (session-flag-set? flag)
    (session-flags (remq flag (session-flags)))))

(define (session-flag-set? flag)
  (memq flag (session-flags)))

(define (print-session-flags)
  (dev-note "print-session-flags:")
  (displayln (session-flags)))

(define current-session-times-in-combat (make-parameter 0))
(define (current-session-times-in-combat++)
  (current-session-times-in-combat (add1 (current-session-times-in-combat))))
