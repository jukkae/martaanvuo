#lang racket/base

(define (print-inventory inventory)
  (newline)
  (displayln "You ponder your earthly possessions.")
  (newline)
  (displayln (string-append "In addition to clothes, you have " inventory ".")))

(provide (all-defined-out))