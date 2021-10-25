#lang racket

(provide (all-defined-out))

(require racket/serialize)

(require "io.rkt"
         "utils.rkt")


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

    ['ration
     (new-item
      "Food rations"
      #:id id
      #:details amount)]

    ['knife
     (new-item
      "Knife"
      #:id id)]

    ['rope
     (new-item
      "Rope"
      #:id id)]

    ['flashlight
     (new-item
      "Flashlight"
      #:id id
      #:details 34)] ; charge percentage

    ['gold
     (new-item
      "Gold" ; gold-198, to be more precise
      #:id id
      #:details amount)] ; amount in grams, for now
    
    [else (dev-note "make-item: unknown id:") (prln id)]))

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
                             (string-append " " "Cuts, breaks, crushes and cracks." " ")))
                           ((eq? (item-id item) 'ration)
                            (list
                             (string-append " " (item-name item) " ")
                             (string-append " " (~v (item-details item)) " ")))
                           ((item? item)
                            (list
                             (string-append " " (item-name item) " ")
                             (string-append " " (~v (item-details item)) " ")))
                           (else
                            (list
                             (string-append " " (symbol->string item) " ")
                             (string-append " " " " " "))))))
  (info-card body title))

(define (weapon-info gun)
  (define body
    (append-element
     (for/list ([item-detail (item-details gun)])
       (list (string-append " "
                            (car item-detail)
                            " ")
             (string-append " "
                            (~s (cdr item-detail))
                            " ")))
     (list (string-append " Ammo left ")
           (string-append " "
                          (number->string (ranged-weapon-ammo-left gun))
                          " "))))
  (info-card body (item-name gun)))