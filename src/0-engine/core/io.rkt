#lang at-exp racket

(provide (all-defined-out))
(provide (all-from-out "output.rkt"))
(require "output.rkt")

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

(define *output-state* 'clean) ; not serialized, local
(define *most-recent-output-type* '()) ; not serialized, local

(define (prln args)
  (set! *output-state* 'dirty)
  (displayln args))

(define (info-card content title [padding? #t])
  (set! *output-state* 'dirty)
  (set! *most-recent-output-type* 'info-card)
  (when (not (null? title)) (prln (format "[~a]" title)))
  (when (not (null? (prune content)))
    (if padding?
      (print-table
        content
        #:row-sep? #f

        #:border-style
        '(("┌─" "─" "─┬─" "─┐")
          ("│ " " " " │ " " │")
          ("├─" "─" "─┼─" "─┤")
          ("└─" "─" "─┴─" "─┘"))
        )
      (print-table
        content
        #:row-sep? #f)
    )
    (newline)))

(define (display-title)
  (define title-string
    @~a{
M A R T A A N V U O
===================
    })
  (define s (format-for-printing title-string #:width 84 #:indent 4))
  (print-paragraph s))

(define (title)
  (define width 92) ; 80?

  (br)
  (prln (string-append* "" (make-list width "-")))
  (br)

  (display-title))

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
  (set! *most-recent-output-type* 'paragraph)
  (displayln formatted-text))

(define (repeat-last-paragraph)
  (hr)
  (print-paragraph (format-for-printing (current-last-paragraph) #:width 84 #:indent 4)))

(define (hr)
  ; (prln "---")
  (prln "--------------------------------------------------------------------------------------------")
  (newline))

(define (paragraph #:suppress-logging? [suppress-logging? #f] . args)
  (define previous-output-type *most-recent-output-type*)
  (when (not (eq? 'paragraph previous-output-type))
    (newline))
  (cond ((not (equal? paragraph ""))
         (define str (string-append* args))
         (define s (format-for-printing str #:width 84 #:indent 4))
         (when (not suppress-logging?)
          (current-last-paragraph str)
          (write-paragraph-to-log str)
         )
         (print-paragraph s))
        (else ; don't do anything with empty input
         '())))
(define p paragraph)

(define (notice . args)
  (define str (format "[~a]" (string-append* args)))
  (define s (format-for-printing str #:width 92 #:indent 0))
  (set! *output-state* 'dirty)
  (set! *most-recent-output-type* 'notice)
  (display s))

(define (info-card-header . args)
  (define str (format "[~a]" (string-append* args)))
  (define s (format-for-printing str #:width 92 #:indent 0))
  (set! *output-state* 'dirty)
  (set! *most-recent-output-type* 'info-card)
  (display s))

; only confirm if there's been something new – no redundant confirmations
(define (wait-for-confirm)
  (case *output-state*
   ['dirty
    (newline)
    (prln "[Enter]")
    (newline)
    (define input (read-line))
    (set! *output-state* 'clean)
    input]
   [else '()]))

(define (wait-for-input)
  (define input (read-line))
  (newline)
  input)

(define (write-save-file serialized-state)
  (define output-file (open-output-file "save.txt" #:exists 'truncate)) ; truncate = delete if exists
  (write serialized-state output-file)
  (close-output-port output-file))

(define (delete-save-file)
  (delete-file "save.txt"))
