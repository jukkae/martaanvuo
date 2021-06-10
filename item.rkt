#lang racket

(provide (all-defined-out))

(require racket/serialize)

(serializable-struct
 item
 (id
  [name #:mutable]
  [details #:mutable]))

(define (make-item id)
  (define details '())
  (case id
    ['bolt-cutters
     (define name "Bolt cutters")
     (item id name details)]
    ['revolver
     (define name "Revolver")
     (item id name (list (list "Ammo left" 6)))]
    [else (displayln "make-item: unknown id:") (displayln "id") '()]
  ))