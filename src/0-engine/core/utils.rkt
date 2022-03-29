#lang at-exp racket

(provide (all-defined-out))
(provide (all-from-out "../2-core/core.rkt"))

(require racket/path) ; used in (here)
(require racket/lazy-require)

(require "../2-core/core.rkt")

(lazy-require
 ["io.rkt"
  (p
   wait-for-input)])

(define-syntax (here stx)
  (with-syntax ([file (syntax-source stx)]
                [line (syntax-line stx)])
    (syntax-case stx ()
      [_ #'(begin
             (displayln "HERE")
             (displayln line)
             (displayln (file-name-from-path file)))])))

(define-syntax (dev-note stx)
  (with-syntax ([file (syntax-source stx)]
                [line (syntax-line stx)])
    (syntax-case stx ()
      [(dev-note message)
       #'(begin
           (displayln
            (format "< ~a:~a > ~a"
                    (path->string (find-relative-path (current-directory) file))
                    (number->string line)
                    message)))])))



; various container stuff
(define (collect-similar lst)
  (hash->list
   (foldl (lambda (key ht)
            (hash-update ht key add1 0))
          '#hash()
          lst)))


; menus - rename this!
(define (handle-meta-command meta-commands-with-keys input)
  (set! input (string-upcase input))
  (define meta-command-with-key (hash-ref meta-commands-with-keys input '()))
  (define meta-command (cdr meta-command-with-key))
  (meta-command))
