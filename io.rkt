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



(define (format string #:width width #:indent indent)
  (define line-width width)
  (define space-width 1)

  (define words (string-split string))
  (define space-left line-width)
  (define output "")

  (for ([word words])
    (cond ((> (+ (string-length word) space-width) space-left)
           (set! output (string-append output "\n")) ; append newline
           (define indent-string (make-string indent #\space))
           (set! output (string-append output indent-string)) ; append indenting

           (set! space-left (- line-width (string-length word))))
          (else
           (set! output (string-append output " ")) ; whoops this creates an extra space for the first word
           (set! space-left (- space-left (+ (string-length word) space-width)))))
    (set! output (string-append output word)))

  (set! output (substring output 1)) ; remove the extra space

  output)

(define test-string "foo bar xyzzy")

(define formatted (format test-string #:width 3 #:indent 2))

(displayln formatted)