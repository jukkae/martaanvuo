#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "../io.rkt")
(require "../utils.rkt")

(lazy-require
 ["../situation.rkt" (current-log
                      current-part
                      current-chapter)])

(define (append-to-log paragraph)
  (current-log (append-element (current-log) paragraph)))



(define (next-chapter!)
  (when (= (current-part) 0)
    (current-part 1))
  (current-chapter (add1 (current-chapter)))
  (print-heading))

(define (next-part!)
  (current-part (add1 (current-part)))
  (current-chapter 0)
  (print-heading))