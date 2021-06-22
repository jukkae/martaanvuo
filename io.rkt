#lang racket

(provide (all-defined-out))

(require text-table)

(require "utils.rkt")

(define (info-card content title)
  (when (not (null? title)) (displayln (string-append "[" title "]")))
  (print-table content #:row-sep? #f)
  (newline))

(define *log* '())

(define (write-paragraph-to-log paragraph)
  (set! *log* (append-element *log* paragraph)))

(define (display-log)
  (hr)
  (displayln "[BEGIN LOG]")
  (newline)
  (for ([entry *log*])
    (paragraph entry))
  (displayln "[END LOG]")
  (newline))

; hide this
(define *last-paragraph* '())

(define (repeat-last-paragraph)
  (hr)
  (paragraph *last-paragraph*))

(define (hr)
  (displayln "---")
  (newline))

(define (paragraph . args)
  (set! *last-paragraph* (string-append* args))
  (write-paragraph-to-log (string-append* args))
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