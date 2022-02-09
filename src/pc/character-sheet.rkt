#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require
  "../actors/actor.rkt"
  "../actors/pc-actor.rkt"
  "../items/item.rkt"
  "../core/io.rkt"
  "../core/utils.rkt")

(lazy-require
 ["../state/state.rkt"
  (pc
   )])

(define (character-sheet)
  (define actor (pc))

  (define sheet
    (tbody
     (tr (actor-name actor) "" )
     (tr "" "")
     (tr "HP" (string-append (number->string (actor-hp actor))
                             "/"
                             (number->string (actor-max-hp actor))))))

  (when (not (= 0 (pc-actor-xp actor)))
    (set! sheet (append-element sheet
                                (tr "XP"
                                    (number->string (pc-actor-xp actor))))))

  (define attributes-list '())
  (when (or (not (null? (actor-strength actor)))
            (not (null? (actor-dexterity actor)))
            (not (null? (actor-constitution actor)))
            (not (null? (actor-intelligence actor)))
            (not (null? (actor-charisma actor))))
    (set! attributes-list
          (append-element attributes-list
                          (tr "" ""))))

  (when (not (null? (actor-strength actor)))
    (set! attributes-list (append-element attributes-list
                                          (tr
                                           "strength"
                                           (string-append (number->string (actor-strength actor))
                                                          " ["
                                                          (get-modifier-string
                                                           (get-attribute-modifier-for
                                                            (actor-strength actor)))
                                                          "]")))))
  (when (not (null? (actor-dexterity actor)))
    (set! attributes-list (append-element attributes-list
                                          (tr
                                           "dexterity"
                                           (string-append (number->string (actor-dexterity actor))
                                                          " ["
                                                          (get-modifier-string
                                                           (get-attribute-modifier-for
                                                            (actor-dexterity actor)))
                                                          "]")))))
  (when (not (null? (actor-constitution actor)))
    (set! attributes-list (append-element attributes-list
                                          (tr
                                           "constitution"
                                           (string-append (number->string (actor-constitution actor))
                                                          " ["
                                                          (get-modifier-string
                                                           (get-attribute-modifier-for
                                                            (actor-constitution actor)))
                                                          "]")))))
  (when (not (null? (actor-intelligence actor)))
    (set! attributes-list (append-element attributes-list
                                          (tr
                                           "intelligence"
                                           (string-append (number->string (actor-intelligence actor))
                                                          " ["
                                                          (get-modifier-string
                                                           (get-attribute-modifier-for
                                                            (actor-intelligence actor)))
                                                          "]")))))
  (when (not (null? (actor-charisma actor)))
    (set! attributes-list (append-element attributes-list
                                          (tr
                                           "charisma"
                                           (string-append (number->string (actor-charisma actor))
                                                          " ["
                                                          (get-modifier-string
                                                           (get-attribute-modifier-for
                                                            (actor-charisma actor)))
                                                          "]")))))

  (define traits (actor-traits actor))
  (define traits-list
    (for/list ([(k v) (in-hash traits)])
      (tr k (number->string v))))

  ; append emptyline above
  (when (not (null? traits-list))
    (set! traits-list (cons (tr "" "") traits-list)))

  (define hunger-list
    (list
     (tr "" "")
     (tr "hunger"
         (number->string (pc-actor-hunger actor)))))

  (set! sheet (append sheet attributes-list traits-list hunger-list))
  (info-card
   sheet
   "Character sheet"
   )

  (inventory)
  (wait-for-confirm)
  #t
  )

(define (inventory)
  (define actor (pc))

  (define header
    (tbody
     (tr "Item" "Notes")))

  (define items (actor-inventory actor))
  (define items-list
    (for/list ([item items])
      (cond ((ranged-weapon? item)
             (tr
              (item-name item)
              (string-append "ammo left: " (number->string (ranged-weapon-ammo-left item)))))
            ((eq? (item-id item) 'bolt-cutters)
             (tr
              (item-name item)
              ""))
            ((item? item)
             (tr
              (item-name item)
              (~v (item-details item))))
            (else
             (tr
              (symbol->string item)
              "")))
      ))

  (define sheet
    (append
     header
     items-list))

  (info-card
   sheet
   "Inventory"
   ))
