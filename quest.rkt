#lang racket

(provide (all-defined-out))

(require racket/serialize)

(serializable-struct
 quest
 (id
  [title #:mutable]
  [status #:mutable]
  [notes #:mutable]
  [details #:mutable]))

(define (format-quest-for-card q)
  (list (string-append " "
                         (quest-title q)
                         " ")
          (string-append " "
                         (quest-status q)
                         " ")
          (string-append " "
                         (quest-notes q)
                         " ")))