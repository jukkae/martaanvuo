#lang racket

(provide (all-defined-out))

(require "io.rkt")
(require "fragment.rkt")
(require "fragments.rkt")
(require "pc.rkt")
(require "situation.rkt")

; fragment handler
(define (current-fragment-on-begin-round!)
  (paragraph (story-fragment-description (situation-current-fragment *situation*)))
  )

; fragment handler
(define (current-fragment-get-decisions)
  (filter (lambda (potential-decision)
            ((decision-requirement potential-decision)))
          (story-fragment-decisions (situation-current-fragment *situation*))))

; fragment handler
; move specifics from here to the actual fragment
(define (current-fragment-handle-decision! decision)

  (paragraph (decision-description decision))
  (define next-fragment (decision-next-fragment decision))

  ; brilliant idea x dirty hack
  (when (procedure? next-fragment)
    (set! next-fragment (next-fragment)))
  (cond ((number? next-fragment)
         (go-to-story-fragment next-fragment)
         )
        ((eq? 'exit next-fragment)
         (set-situation-current-fragment! *situation* '()))
        ((eq? 'exit-and-set-build-desperate next-fragment)
         (set-build! 'desperate)
         (set-situation-current-fragment! *situation* '()))
        ((eq? 'exit-and-set-build-bruiser next-fragment)
         (set-build! 'bruiser)
         (set-situation-current-fragment! *situation* '()))
        (else (error (string-append "(current-fragment-handle-decision!): next-fragment type not implemented: " (symbol->string next-fragment)))))
  )

; fragment handler
(define (current-fragment-on-end-round!)
  '()
  )

; fragment handler
(define (go-to-story-fragment id)
  (set-situation-current-fragment! *situation* (get-fragment id))
  ((story-fragment-on-enter! (situation-current-fragment *situation*))))