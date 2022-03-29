#lang at-exp racket

(provide (all-defined-out))


(require
  "../2-core/io.rkt"
  "../2-core/core.rkt"
  "../3-types/item.rkt")


(define (new-item
         name
         #:id id
         #:details (details '())
         #:quantity (quantity 1))
  (item* id name details quantity))

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
      #:quantity 2)]

    ['ration
     (new-item
      "Food rations"
      #:id id
      #:quantity amount)]

    ['fresh-berries
     (new-item
      "Berries, fresh"
      #:id id
      #:quantity amount)]

    ['vatruska
     (new-item
      "Vatruska"
      #:id id
      #:quantity amount)]

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
      #:quantity amount)] ; amount in grams, for now

    [else
     (new-item
      (capitalize-first-letter (symbol->string id))
      #:id id
      #:quantity amount)]))

(define (increase-ammo! gun)
  (set-ranged-weapon-ammo-left! gun (add1 (ranged-weapon-ammo-left gun))))

(define (item-info-card
         item
         #:title [title "Item"])

  (define body (tbody (cond ((ranged-weapon? item)
                             (tr
                              (item-name item)
                              (format "ammo left: ~a" (ranged-weapon-ammo-left item))))
                            ((eq? (item-id item) 'bolt-cutters)
                             (tr
                              (item-name item)
                              "Cuts, breaks, crushes and cracks."))
                            ((eq? (item-id item) 'ration)
                             (tr
                              (item-name item)
                              (~v (item-details item))))
                            ((eq? (item-id item) 'fresh-berries)
                             (define quantity-text (if (= (item-quantity item) 1)
                                                       "handful"
                                                       "handfuls"))
                             (tr
                              (item-name item)
                              (format "~a ~a" (item-quantity item) quantity-text)))
                            ((item? item)
                             (tr
                              (item-name item)
                              (~v (item-details item))))
                            (else
                             (tr
                              (symbol->string item)
                              "NA")))))
  (info-card body title))

(define (weapon-info gun)
  (define body
    (append-element
     (for/list ([item-detail (item-details gun)])
       (tr (car item-detail)
           (~s (cdr item-detail))))
     (tr "Ammo left"
         (number->string (ranged-weapon-ammo-left gun)))))
  (info-card body (item-name gun)))
