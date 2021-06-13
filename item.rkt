#lang racket

(provide (all-defined-out))

(require racket/serialize)

(serializable-struct
 item
 (id
  [name #:mutable]
  [details #:mutable]))

(serializable-struct
 ranged-weapon
 item
 ([ammo-left #:mutable]))

(define (make-item id)
  (define details '())
  (case id
    ['bolt-cutters
     (define name "Bolt cutters")
     (item id name details)]
    ['revolver
     (define name "Revolver")
     (ranged-weapon id name details 3)]
    [else (displayln "make-item: unknown id:") (displayln "id") '()]
  ))