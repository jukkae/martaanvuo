#lang racket/base

(require roman-numeral)
(require "utils.rkt")

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
  (displayln "You should be able to select a previous save. You can not.")
  (newline)
  (displayln "A new game begins."))

(define (get-curse)
  (define index 0 #;(random 2))
  (cond ((= index 0) (define first '("Rot" "Blight" "Pus" "Pain" "Snow" "Rain" "Frost"))
                     (define second '("decay" "corrosion" "death" "destruction" "sorrow" "suffering"))
                     (string-append (take-random first) " and " (take-random second) "!"))
        (else (take-random '("Let it all wither!"
                             "May it all languish!"
                             "Blight!"
                             "Scales of a snake!")))))

(provide (all-defined-out))