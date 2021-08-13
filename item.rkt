#lang racket

(provide (all-defined-out))

(require racket/serialize)

(require "io.rkt")


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
(define (make-item
         id
         #:amount [amount 1])
  (case id
    ['bolt-cutters
     (new-item
      "Bolt cutters"
      #:id id)]
    
    ['revolver
     (new-ranged-weapon
      "Revolver"
      #:id 'revolver
      #:ammo-left 3)]

    ['ammo
     (new-item
      "Ammo"
      #:id id
      #:details 2)]

    ['gold
     (new-item
      "Gold" ; gold-198, to be more precise
      #:id id
      #:details amount)] ; amount in grams, for now
    
    [else (displayln "make-item: unknown id:") (displayln "id") '()]))

(define (increase-ammo! gun)
  (set-ranged-weapon-ammo-left! gun (add1 (ranged-weapon-ammo-left gun))))

(define (item-info-card
         item
         #:title [title "Item"])

  (define body (list (cond ((ranged-weapon? item)
                            (list
                             (string-append " " (item-name item) " ")
                             (string-append " " "ammo left: " (number->string (ranged-weapon-ammo-left item)) " ")))
                           ((eq? (item-id item) 'bolt-cutters)
                            (list
                             (string-append " " (item-name item) " ")
                             (string-append " " "Cuts, breaks, cracks, and in a pinch, levers." " ")))
                           ((item? item)
                            (list
                             (string-append " " (item-name item) " ")
                             (string-append " " (~v (item-details item)) " ")))
                           (else
                            (list
                             (string-append " " (symbol->string item) " ")
                             (string-append " " " " " "))))))
  (info-card body title))