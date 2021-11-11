#lang racket

(provide (all-defined-out))

(define current-session-score-dice (make-parameter 1))
(define (current-session-score-dice++)
  (current-session-score-dice (add1 (current-session-score-dice))))