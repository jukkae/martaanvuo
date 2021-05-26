#lang racket

(provide paragraph)
(define (paragraph . args)
  (displayln (string-append* args))
  (newline))