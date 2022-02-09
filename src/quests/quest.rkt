#lang at-exp racket

(provide (all-defined-out))

(require racket/serialize)

(serializable-struct
 quest
 (id
  [title #:mutable]
  [status #:mutable]
  [notes #:mutable]
  [details #:mutable])
 #:transparent)

(define (format-quest-for-card q)
  (list (format "~a" (quest-title q))
        (format "~a" (quest-status q))
        (format "~a" (quest-notes q))))
