#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "../../2-core/io.rkt"
         "../../2-core/core.rkt"
         "../../3-types/item.rkt"
         )

(lazy-require ["../pc/pc.rkt" (pc-has-item? remove-item!)])

(define (new-item name #:id id #:details (details '()) #:quantity (quantity 1) #:interaction-verbs [interaction-verbs '()])
  (item* id name details quantity interaction-verbs))

(define (new-ranged-weapon name #:id id #:details (details '()) #:ammo-left ammo-left #:interaction-verbs [interaction-verbs '()])
  (ranged-weapon* id name details 1 interaction-verbs ammo-left))

; TODO: items = "content cards"
(define (make-item id #:amount [amount 1] #:details [details '()])

  (case id
    ['shaman-bag (new-item "Shaman bag" #:id id)]

    ['knife (new-item "Knife" #:id id)]

    ['lighter (new-item "Lighter" #:id id)]

    ['bolt-cutters (new-item "Bolt cutters" #:id id)]

    ['revolver (new-ranged-weapon "Revolver" #:id 'revolver #:ammo-left 3)]

    ['ammo (new-item "Ammo" #:id id #:quantity 2)]

    ['ration (new-item "Food rations" #:id id #:quantity amount)]

    ['fresh-berries (new-item "Berries, fresh" #:id id #:quantity amount)]

    ['vatruska (new-item "Vatruska" #:id id #:quantity amount)]

    ['knife (new-item "Knife" #:id id)]

    ['rope (new-item "Rope" #:id id)]

    ['flashlight (new-item "Flashlight" #:id id #:details 34)] ; charge percentage

    ['empty-flashlight (new-item "Flashlight (no batteries)" #:id id #:details 0 #:interaction-verbs (list "Turn on" "Change battery"))] ; charge percentage

    ['gold (new-item "gold" #:id id #:quantity amount)]

    ['lucky-charm-slot-machine
     (new-item "Lucky charm (slot machine)" #:id 'lucky-charm-slot-machine #:details 'switched-off #:interaction-verbs (list "Flick the switch"))]

    [else (new-item (capitalize-first-letter (symbol->string id)) #:id id #:quantity amount)]))

(define (increase-ammo! gun)
  (set-ranged-weapon-ammo-left! gun (add1 (ranged-weapon-ammo-left gun))))

(define (item-info-card item #:title [title "Item"])

  (define body
    (tbody (cond
             [(ranged-weapon? item)
              (tr (item-name item) (format "ammo left: ~a" (ranged-weapon-ammo-left item)))]
             [(equal? (item-id item) 'bolt-cutters)
              (tr (item-name item) "Cuts, breaks, crushes and cracks.")]
             [(equal? (item-id item) 'ration) (tr (item-name item) (~v (item-details item)))]
             [(equal? (item-id item) 'fresh-berries)
              (define quantity-text (if (= (item-quantity item) 1) "handful" "handfuls"))
              (tr (item-name item) (format "~a ~a" (item-quantity item) quantity-text))]
             [(item? item) (tr (item-name item) (~v (item-details item)))]
             [else (tr (symbol->string item) "NA")])))
  (info-card body title))

(define (weapon-info gun)
  (define body
    (append-element (for/list ([item-detail (item-details gun)])
                      (tr (car item-detail) (~s (cdr item-detail))))
                    (tr "Ammo left" (number->string (ranged-weapon-ammo-left gun)))))
  (info-card body (item-name gun)))

(define (inspect-item the-item)
  (displayln (format "it is: ~a" the-item)))

(define (interact-with-item the-verb the-item)
  (p (format "Otava tries to ~a the ~a." (string-downcase the-verb) (string-downcase (item-name the-item))))
  (match (item-id the-item)
    ['lucky-charm-slot-machine
     (when (eq? (item-details the-item) 'switched-on)
        (set-item-details! the-item 'switched-off)
        (p "Otava sets the little switch on the slot machine charm to 'off'. Not surprisingly, nothing seems to happen.")
        (wait-for-confirm))
     (when (eq? (item-details the-item) 'switched-off)
        (set-item-details! the-item 'switched-on)
        (p "Otava sets the little switch on the slot machine talisman to 'on'. Nothing seems to happen.")
        (wait-for-confirm))
     ]
    ['empty-flashlight
     (match the-verb
      ["Turn on"
        (cond
         [else (notice "It has no batteries.")]
         )]
      ["Change battery"
        (cond
         [(pc-has-item? 'battery)
          (notice "Otava changes the battery of the flashlight.")
          (remove-item! 'battery #:quantity-to-remove 1)
          (set-item-id! the-item 'flashlight-off)
          (set-item-name! the-item "Flashlight")
          (set-item-details! the-item 43) ; percent
          ]
         [else (notice "Otava has no batteries.")])]
     )]
    ['flashlight-off
     (match the-verb
      ["Turn on"
        (notice "Otava turns on the flashlight.")
        (set-item-id! the-item 'flashlight-on)
        (set-item-name! the-item "Flashlight (on)")
        ]
     )]

    ['flashlight-on
     (match the-verb
      ["Turn off"
        (notice "Otava turns off the flashlight.")
        (set-item-id! the-item 'flashlight-off)
        (set-item-name! the-item "Flashlight (off)")
        ]
     )]
    )
  )
