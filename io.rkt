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
    (print-paragraph (format-for-printing entry #:width 84 #:indent 4)))
  (displayln "[END LOG]")
  (newline)
  (wait-for-confirm))

; implementation detail
(define *last-paragraph* '())

; implementation detail
(define (print-paragraph formatted-text)
  (displayln formatted-text)
  )

(define (repeat-last-paragraph)
  (hr)
  (print-paragraph (format-for-printing *last-paragraph* #:width 84 #:indent 4)))

(define (hr)
  ; (displayln "---")
  (displayln "--------------------------------------------------------------------------------------------")
  (newline))

(define (paragraph . args)
  (define str (string-append* args))
  (define s (format-for-printing str #:width 84 #:indent 4))
  (set! *last-paragraph* str)
  (write-paragraph-to-log str)
  (print-paragraph s))

(define (notice . args)
  (define str (string-append* args))
  (set! str (string-append "[" str "]"))
  (define s (format-for-printing str #:width 92 #:indent 0))
  (print-paragraph s))

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
(define (format-for-printing input-string #:width width #:indent indent)
  (define line-width width)
  (define space-width 1)
  (define indent-string (make-string indent #\space))

  (define final-output "")

  ; if the input begins with a newline, then it is important and should be preserved
  (define first-char (string-ref input-string 0))
  (when (= 10 (char->integer first-char))
    #;(displayln "HELLO YEAH")
    (set! final-output "\n"))
  
  (define pre-broken-lines (string-split input-string "\n"))
  
  (for ([line pre-broken-lines])
    (define output "")
    (define words (string-split line))
    (define space-left line-width)
  
    (for ([word words])
      (cond ((> (+ (string-length word) space-width) space-left)
             (set! output (string-append output "\n")) ; append newline
             (set! output (string-append output indent-string)) ; append indenting

             (set! space-left (- line-width (string-length word))))
            (else
             (set! output (string-append output " ")) ; whoops, this creates an extra space for the first word
             (set! space-left (- space-left (+ (string-length word) space-width)))))
      (set! output (string-append output word)))

    (when (not (equal? "" output)) (set! output (substring output 1))) ; remove the extra space
    (set! output (string-append indent-string output)) ; fix indenting
    (set! final-output (string-append final-output output))
    (set! final-output (string-append final-output "\n")) ; add newline to correspond to the possible manually broken newline, or for paragraph end
    )
  final-output)

(define (write-save-file serialized-state)
  (define output-file (open-output-file "save.txt"))
  (write serialized-state output-file)
  (close-output-port output-file))