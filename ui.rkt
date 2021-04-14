#lang racket

(provide (all-defined-out))

(require "action.rkt")
(require "narration.rkt")



(define (print-choices-with-keys choices-with-keys)
  ; TODO: Should order here based on key
  (for ([(k v) (in-hash choices-with-keys)])
    (displayln (string-append "[" (number->string k) "]: " (choice-name v))))
  (newline))

(define (key-from-index i)
  (cond ((< i 0) (error "negative index!"))
        ((<= i 8) (add1 i))
        ((= i 9) 0)
        ((> i 9) (error "too many things to do!"))))

(define (build-keys-to-choices-map choices)
  (define choices-with-keys (make-hash))
  (for ([i (in-range (length choices))])
    (define key (key-from-index i))
    (hash-set! choices-with-keys key (list-ref choices i)))
  choices-with-keys)

(define (wait-for-confirm)
  (newline)
  (displayln "[Enter]")
  (newline)
  (define input (read-line))
  input)