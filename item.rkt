#lang racket

(provide (all-defined-out))

(require racket/serialize)


(serializable-struct
 item
 (id
  [name #:mutable]
  [details #:mutable])
 #:constructor-name item*)

; not part of API
(define (new-item
         name
         #:id id
         #:details (details '()))
  (item* id name details))

(serializable-struct
 ranged-weapon
 item
 ([ammo-left #:mutable])
 #:constructor-name ranged-weapon*)

; not part of API
(define (new-ranged-weapon
         name
         #:id id
         #:details (details '())
         #:ammo-left ammo-left)
  (ranged-weapon* id name details ammo-left))


; API
(define (make-item id)
  (case id
    ['bolt-cutters
     (new-item
      "Bolt cutters"
      #:id id)]
    
    ['revolver
     (new-ranged-weapon
      "Revolver"
      #:id 'revolver
      #:ammo-left 5)]

    ['ammo
     (new-item
      "Ammo"
      #:id id
      #:details 3)]
    
    [else (displayln "make-item: unknown id:") (displayln "id") '()]))