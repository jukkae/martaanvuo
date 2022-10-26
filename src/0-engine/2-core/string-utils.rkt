#lang at-exp racket

(provide (all-defined-out))

(define (capitalize-first-letter str)
  (cond
    [(non-empty-string? str)
     (define first-letter-str (substring str 0 1))
     (define rest-str (substring str 1 (string-length str)))
     (string-append (string-upcase first-letter-str) rest-str)]
    [else ""]))
