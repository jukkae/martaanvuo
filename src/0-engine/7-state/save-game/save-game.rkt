#lang at-exp racket

(provide (all-defined-out))

(define (write-save-file serialized-state)
  (define output-file (open-output-file "save.txt" #:exists 'truncate)) ; truncate = delete if exists
  (write serialized-state output-file)
  (close-output-port output-file))

(define (delete-save-file)
  (delete-file "save.txt"))
