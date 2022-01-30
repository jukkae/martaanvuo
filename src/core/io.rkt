#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require text-table)

(require "utils.rkt")

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

(define *output-state* 'clean) ; not serialized, local

(define prln displayln)
(define br newline)

(define (info-card content title)
  (when (not (null? title)) (prln (format "[~a]" title)))
  (when (not (null? (prune content)))
    (print-table content #:row-sep? #f)
    (newline)))

(define tbody list)
(define tr list)

(define (write-paragraph-to-log paragraph)
  (append-to-log paragraph))

(define (display-title)
  (define title-string
    (string-append "M A R T A A N V U O"
                   "\n"
                   "==================="))
  (define s (format-for-printing title-string #:width 84 #:indent 4))
  (print-paragraph s))

(define (title)
  (define width 92) ; 80?

  (br)
  (prln (string-append* "" (make-list width "-")))
  (br)

  (display-title))

(define (display-log)
  (hr)
  (prln "[BEGIN LOG]")
  (newline)
  (display-title)
  #;(prln (current-log))
  (for ([entry (current-log)])
    (print-paragraph (format-for-printing entry #:width 84 #:indent 4)))
  (prln "[END LOG]")
  (newline)
  (wait-for-confirm))

(define (display-prompt)
  (newline)
  (prln (current-prompt)))

(define (print-heading)
  (define heading
    (format "\nPART ~a, CHAPTER ~a"
            (number->string (current-part))
            (number->string (current-chapter))))
  (p heading))

; implementation detail
(define (print-paragraph formatted-text)
  (set! *output-state* 'dirty)
  (displayln formatted-text))

(define (repeat-last-paragraph)
  (hr)
  (print-paragraph (format-for-printing (current-last-paragraph) #:width 84 #:indent 4)))

(define (hr)
  ; (prln "---")
  (prln "--------------------------------------------------------------------------------------------")
  (newline))

(define (paragraph . args)
  (cond ((not (equal? paragraph ""))
         (define str (string-append* args))
         (define s (format-for-printing str #:width 84 #:indent 4))
         (current-last-paragraph str)
         (write-paragraph-to-log str)
         (print-paragraph s))
        (else ; don't do anything with empty input
         '())))
(define p paragraph)

(define (notice . args)
  (define str (string-append* args))
  (set! str (string-append "[" str "]"))
  (define s (format-for-printing str #:width 92 #:indent 0))
  (print-paragraph s))

; only confirm if there's been something new – no redundant confirmations
(define (wait-for-confirm)
  (case *output-state*
   ['dirty
    (prln "[Enter]")
    (newline)
    (define input (read-line))
    (set! *output-state* 'clean)
    input
   ]
   [else '()]))

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

(define (write-save-file serialized-state)
  (define output-file (open-output-file "save.txt" #:exists 'truncate)) ; truncate = delete if exists
  (write serialized-state output-file)
  (close-output-port output-file))

(define (delete-save-file)
  (delete-file "save.txt"))
