#lang typed/racket

(provide Maybe)

(define-type (Maybe a) (U '() a))
