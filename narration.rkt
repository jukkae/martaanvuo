#lang racket

(require roman-numeral)
(require "utils.rkt")

(define (paragraph . args)
  (displayln (string-append* args))
  (newline))

; TODO: define in terms of describable<%>
(define (get-list-inline-description list)
  (if (empty? list)
      "nothing"
      (string-append (send (car list) get-inline-description)
                     (cond ((= (length list) 1) "")
                           ((= (length list) 2) (string-append " and " (get-list-inline-description (cdr list))))
                           (else (string-append ", " (get-list-inline-description (cdr list))))))))

(define (get-string-list-inline-description list)
  (if (empty? list)
      "nothing"
      (string-append (car list)
                     (cond ((= (length list) 1) "")
                           ((= (length list) 2) (string-append " and " (get-string-list-inline-description (cdr list))))
                           (else (string-append ", " (get-string-list-inline-description (cdr list))))))))

(define (print-inventory inventory)
  (newline)
  (displayln "You ponder your earthly possessions.")
  (newline)
  (cond ((eq? inventory '()) (displayln "Or, you would, if you actually possessed anything."))
        (else
         (define sorted (collect-similar inventory))
         (displayln sorted))))

(define (title)
  (newline)
  (displayln "M A R T A A N V U O")
  (displayln "==================="))



(provide (all-defined-out))