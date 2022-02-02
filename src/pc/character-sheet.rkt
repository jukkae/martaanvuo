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
    (list
     (list (string-append " " (actor-name actor) " ") "" )
     (list "" "")
     (list " HP " (string-append " "
                                 (number->string (actor-hp actor))
                                 "/"
                                 (number->string (actor-max-hp actor))
                                 " "))))

  (when (not (= 0 (pc-actor-xp actor)))
    (set! sheet (append-element sheet
                                (list " XP " (string-append " "
                                                            (number->string (pc-actor-xp actor))
                                                            " ")))))

  (define attributes-list '())
  (when (or (not (null? (actor-strength actor)))
            (not (null? (actor-dexterity actor)))
            (not (null? (actor-constitution actor)))
            (not (null? (actor-intelligence actor)))
            (not (null? (actor-charisma actor))))
    (set! attributes-list
          (append-element attributes-list
                          (list "" ""))))

  (when (not (null? (actor-strength actor)))
    (set! attributes-list (append-element attributes-list
                                          (list
                                           " strength "
                                           (string-append " "
                                                          (number->string (actor-strength actor))
                                                          " ["
                                                          (get-modifier-string
                                                           (get-attribute-modifier-for
                                                            (actor-strength actor)))
                                                          "] ")))))
  (when (not (null? (actor-dexterity actor)))
    (set! attributes-list (append-element attributes-list
                                          (list
                                           " dexterity "
                                           (string-append " "
                                                          (number->string (actor-dexterity actor))
                                                          " ["
                                                          (get-modifier-string
                                                           (get-attribute-modifier-for
                                                            (actor-dexterity actor)))
                                                          "] ")))))
  (when (not (null? (actor-constitution actor)))
    (set! attributes-list (append-element attributes-list
                                          (list
                                           " constitution "
                                           (string-append " "
                                                          (number->string (actor-constitution actor))
                                                          " ["
                                                          (get-modifier-string
                                                           (get-attribute-modifier-for
                                                            (actor-constitution actor)))
                                                          "] ")))))
  (when (not (null? (actor-intelligence actor)))
    (set! attributes-list (append-element attributes-list
                                          (list
                                           " intelligence "
                                           (string-append " "
                                                          (number->string (actor-intelligence actor))
                                                          " ["
                                                          (get-modifier-string
                                                           (get-attribute-modifier-for
                                                            (actor-intelligence actor)))
                                                          "] ")))))
  (when (not (null? (actor-charisma actor)))
    (set! attributes-list (append-element attributes-list
                                          (list
                                           " charisma "
                                           (string-append " "
                                                          (number->string (actor-charisma actor))
                                                          " ["
                                                          (get-modifier-string
                                                           (get-attribute-modifier-for
                                                            (actor-charisma actor)))
                                                          "] ")))))

  (define traits (actor-traits actor))
  (define traits-list
    (for/list ([(k v) (in-hash traits)])
      (list (string-append " " k " ") (string-append " " (number->string v) " "))))

  ; append emptyline above
  (when (not (null? traits-list))
    (set! traits-list (cons (list "" "") traits-list)))

  (define hunger-list
    (list
     (list "" "")
     (list " hunger "
           (string-append " "
                          (number->string (pc-actor-hunger actor))
                          " "))))

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
    (list
     (list " Item " " Notes ")))

  (define items (actor-inventory actor))
  (define items-list
    (for/list ([item items])
      (cond ((ranged-weapon? item)
             (list
              (string-append " " (item-name item) " ")
              (string-append " " "ammo left: " (number->string (ranged-weapon-ammo-left item)) " ")))
            ((eq? (item-id item) 'bolt-cutters)
             (list
              (string-append " " (item-name item) " ")
              (string-append "")))
            ((item? item)
             (list
              (string-append " " (item-name item) " ")
              (string-append " " (~v (item-details item)) " ")))
            (else
             (list
              (string-append " " (symbol->string item) " ")
              (string-append " " " " " "))))
      ))

  (define sheet
    (append
     header
     items-list))

  (info-card
   sheet
   "Inventory"
   ))
