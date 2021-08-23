#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "../utils.rkt")

(lazy-require
 ["../situation.rkt" (current-log)])

(define (append-to-log paragraph)
  (current-log (append-element (current-log) paragraph)))