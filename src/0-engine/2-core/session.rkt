#lang at-exp racket

(provide (all-defined-out))

(require
  "dev-note.rkt"
  "dice.rkt"
  "list-utils.rkt"
  "menu-utils.rkt"
  "output.rkt"
  "string-utils.rkt"
  )

(define current-session-score-dice (make-parameter 0))
(define (current-session-score-dice++ [reason '()])
  (current-session-score-dice (add1 (current-session-score-dice)))
  (when (not (null? reason))
    (current-session-score-reasons (append-element (current-session-score-reasons) reason))))

(define current-session-score-reasons (make-parameter '()))

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
  (dev-note (format "print-session-flags: ~a" session-flags)))

(define current-session-times-in-combat (make-parameter 0))
(define (current-session-times-in-combat++)
  (current-session-times-in-combat (add1 (current-session-times-in-combat))))
