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

; implementation detail
(define *last-paragraph* '())

(define (repeat-last-paragraph)
  (hr)
  (paragraph *last-paragraph*))

(define (hr)
  (displayln "---")
  (newline))

(define (paragraph . args)
  (define str (string-append* args))
  (define s (format-for-printing str #:width 84 #:indent 4))
  (set! *last-paragraph* s)
  (write-paragraph-to-log s)
  (displayln s)
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

; implementation detail
(define (format-for-printing string #:width width #:indent indent)
  (define line-width width)
  (define space-width 1)

  (define indent-string (make-string indent #\space))

  (define words (string-split string))
  (define space-left line-width)
  (define output "")

  (for ([word words])
    (cond ((> (+ (string-length word) space-width) space-left)
           (set! output (string-append output "\n")) ; append newline
           (set! output (string-append output indent-string)) ; append indenting

           (set! space-left (- line-width (string-length word))))
          (else
           (set! output (string-append output " ")) ; whoops, this creates an extra space for the first word
           (set! space-left (- space-left (+ (string-length word) space-width)))))
    (set! output (string-append output word)))

  (set! output (substring output 1)) ; remove the extra space
  (set! output (string-append indent-string output)) ; fix indenting

  output)