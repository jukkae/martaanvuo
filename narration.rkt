#lang racket

(require roman-numeral)
(require "utils.rkt")

(define (paragraph . args)
  (newline) ; TODO either newline's got to go
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

(define (title)
  (newline)
  (displayln "M A R T A A N V U O")
  (displayln "==================="))

(define (print-inventory inventory)
  (newline)
  (displayln "You ponder your earthly possessions.")
  (newline)
  (displayln (string-append "In addition to clothes, you have " inventory ".")))

(define (narrate-run-number number)
  (newline)
  (newline)
  (displayln (string-append "RUN " (string-upcase (number->roman number))))
  (newline)
  (displayln "A sense of self emerges from the Dark. You arise in")
  (displayln "M A R T A A N V U O.")
  (newline))

(define (narrate-startup)
  (newline)
  (displayln "You should be able to select a previous save. [[Not implemented yet]]")
  (newline)
  (displayln "A new game begins."))

(define (narrate-quit)
  (displayln "You quit.")
  (newline)
  (displayln "Your progress should be saved. [[Not implemented yet]]"))

(define (get-curse)
  (take-random '("Death and decay"
                 "Pain and suffering"
                 "Rot and corrosion")))

(provide (all-defined-out))