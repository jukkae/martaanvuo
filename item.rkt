#lang racket

(provide (all-defined-out))

(require racket/serialize)

; TODO fields like this:
; name, by position, required
; id, by name, required
; details, by name, not required, default to '()
(serializable-struct
 item
 (id
  [name #:mutable]
  [details #:mutable]))

(define (new-item
         name
         #:id id
         #:details (details '()))
  (item id name details))

(serializable-struct
 ranged-weapon
 item
 ([ammo-left #:mutable]))

(define (new-ranged-weapon
         name
         #:id id
         #:details (details '())
         #:ammo-left ammo-left)
  (ranged-weapon id name details ammo-left))

(define (make-item id)
  (case id
    ['bolt-cutters
     (define name "Bolt cutters")
     (new-item
      "Bolt cutters"
      #:id id)]
    
    ['revolver
     (new-ranged-weapon
      "Revolver"
      #:id 'revolver
      #:ammo-left 3)]
    
    [else (displayln "make-item: unknown id:") (displayln "id") '()]))