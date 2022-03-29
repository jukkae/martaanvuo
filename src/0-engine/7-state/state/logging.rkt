#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "../../2-core/io.rkt"
         "../../2-core/core.rkt")

(lazy-require
 ["state.rkt" (current-log
               current-part
               current-chapter
               current-prompt)])

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

(define (set-prompt! prompt)
  (current-prompt prompt))
