#lang racket

(provide (all-defined-out))

(require text-table)

(define (info-card content title)
  (when (not (null? title)) (displayln (string-append "[" title "]")))
  (print-table content #:row-sep? #f)
  (newline))

; hide this
(define *last-paragraph* '())

(define (hr)
  (displayln "---")
  (newline))

(define (paragraph . args)
  (set! *last-paragraph* (string-append* args))
  (displayln (string-append* args))
  (newline))

(define (wait-for-confirm)
  (displayln "[Enter]")
  (newline)
  (define input (read-line))
  input)

(define (wait-for-input)
  (newline)
  (define input (read-line))
  (newline)
  input)