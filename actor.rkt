#lang racket

(define actor<%>
  (interface ()
    get-next-command))

(provide (all-defined-out))