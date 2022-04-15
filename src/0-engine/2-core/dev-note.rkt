#lang at-exp racket

(provide (all-defined-out))

(require "../../metadata.rkt")

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
                    (some-system-path->string (find-relative-path project-root-path file))
                    (number->string line)
                    message)))])))