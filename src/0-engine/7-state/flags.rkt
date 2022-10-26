#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "../2-core/io.rkt"
         "../2-core/core.rkt")

(lazy-require ["state.rkt" (current-flags)])

(define (set-flag flag)
  (when (not (flag-set? flag))
    (current-flags (append-element (current-flags) flag))))

(define (remove-flag flag)
  (when (flag-set? flag)
    (current-flags (remq flag (current-flags)))))

(define (flag-set? flag)
  (memq flag (current-flags)))

(define (toggle-flag flag)
  (if (flag-set? flag) (remove-flag flag) (set-flag flag)))

(define (print-flags)
  (dev-note (format "flags: ~a" (current-flags))))
