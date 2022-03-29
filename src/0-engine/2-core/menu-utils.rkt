#lang at-exp racket

(provide (all-defined-out))

(define (handle-meta-command meta-commands-with-keys input)
  (set! input (string-upcase input))
  (define meta-command-with-key (hash-ref meta-commands-with-keys input '()))
  (define meta-command (cdr meta-command-with-key))
  (meta-command))
