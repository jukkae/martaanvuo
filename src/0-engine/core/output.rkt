#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)
(require text-table)

(require "../2-core/list-utils.rkt")

(lazy-require
 ["../state/state.rkt"
  (current-part
   current-chapter
   current-log
   current-last-paragraph
   current-prompt)])

(lazy-require
 ["../state/logging.rkt"
  (append-to-log)])

; implementation detail
(define (format-for-printing input-string #:width width #:indent indent)
  (define line-width width)
  (define space-width 1)
  (define indent-string (make-string indent #\space))

  (define final-output "")

  ; if the input begins with a newline, then it is important and should be preserved
  (define first-char (string-ref input-string 0))
  (when (= 10 (char->integer first-char))
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
    (set! final-output (string-append final-output "\n"))) ; add newline to correspond to the possible manually broken newline, or for paragraph end

  final-output)

(define br newline)

(define tbody list)
(define tr list)

(define (write-paragraph-to-log paragraph)
  (append-to-log paragraph))
