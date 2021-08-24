#lang racket


(provide (all-defined-out))

(require racket/lazy-require)

(require "../utils.rkt")

(lazy-require
 ["../situation.rkt" (current-flags
                      current-in-combat?)])

;; bad name for this file, this is sort of "misc"

(define (set-flag flag)
  (when (not (flag-set? flag))
    (current-flags (append-element (current-flags) flag))))

(define (remove-flag flag)
  (when (flag-set? flag)
    (current-flags (remq flag (current-flags)))))

(define (flag-set? flag)
  (memq flag (current-flags)))

(define (print-flags)
  (dev-note "print-flags:")
  (displayln (current-flags)))


(define (in-combat?) (current-in-combat?))