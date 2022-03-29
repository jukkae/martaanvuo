#lang at-exp racket

(provide character-sheet
         display-inventory)

(require racket/lazy-require)

(require
  "../../2-core/io.rkt"
  "../../2-core/core.rkt"
  "../../3-types/actor.rkt"
  "../../3-types/pc-actor.rkt"
  "../../3-types/condition.rkt"
  "../../3-types/item.rkt"
  )

(lazy-require
 ["../../6-state/state/state.rkt"
  (pc
   )])

; Eventually, maybe lift this to the same "level" as numbered options. Then:
; Character sheet could be a treelike menu, where also letters are used.
; '-' is used to go back one level.
; Then, concrete actions with diegetic effects should return an action ultimately for (get-next-pc-action).
; Otherwise, return #t or whatever's appropriate for round-resolver
(define (character-sheet)
  (display-character-sheet)
  (display-inventory)
  (wait-for-confirm)
  #t
  )

(define (display-character-sheet)
  (define actor (pc))

  (define sheet
    (tbody
     (tr (actor-name actor) "" )
     (tr "" "")
     (tr "HP" (format "~a/~a" (actor-hp actor) (actor-max-hp actor)))))

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
                                           (format "~a [~a]"
                                                   (actor-strength actor)
                                                   (get-modifier-string
                                                    (get-attribute-modifier-for
                                                     (actor-strength actor))))))))
  (when (not (null? (actor-dexterity actor)))
    (set! attributes-list (append-element attributes-list
                                          (tr
                                           "dexterity"
                                           (format "~a [~a]"
                                                   (actor-dexterity actor)
                                                   (get-modifier-string
                                                    (get-attribute-modifier-for
                                                     (actor-dexterity actor))))))))
  (when (not (null? (actor-constitution actor)))
    (set! attributes-list (append-element attributes-list
                                          (tr
                                           "constitution"
                                           (format "~a [~a]"
                                                   (actor-constitution actor)
                                                   (get-modifier-string
                                                    (get-attribute-modifier-for
                                                     (actor-constitution actor))))))))
  (when (not (null? (actor-intelligence actor)))
    (set! attributes-list (append-element attributes-list
                                          (tr
                                           "intelligence"
                                           (format "~a [~a]"
                                                   (actor-intelligence actor)
                                                   (get-modifier-string
                                                    (get-attribute-modifier-for
                                                     (actor-intelligence actor))))))))
  (when (not (null? (actor-charisma actor)))
    (set! attributes-list (append-element attributes-list
                                          (tr
                                           "charisma"
                                           (format "~a [~a]"
                                                   (actor-charisma actor)
                                                   (get-modifier-string
                                                    (get-attribute-modifier-for
                                                     (actor-charisma actor))))))))

  (define traits (actor-traits actor))
  (define traits-list
    (for/list ([(k v) (in-hash traits)])
      (tr k (number->string v))))

  ; append emptyline above
  (when (not (null? traits-list))
    (set! traits-list (cons (tr "" "") traits-list)))


  (define conditions (actor-conditions actor))
  (define conditions-list
    (for/list ([condition conditions])
      (tr (format "[~a]" (condition-type condition)) "")))

  ; append emptyline above
  (when (not (null? conditions-list))
    (set! conditions-list (cons (tr "" "") conditions-list)))


  (define hunger-list
    (list
     (tr "" "")
     (tr "hunger"
         (number->string (pc-actor-hunger actor)))))

  (set! sheet (append sheet attributes-list traits-list conditions-list hunger-list))
  (info-card
   sheet
   "Character sheet"
   ))


(define (display-inventory)
  (define actor (pc))

  (define header
    (tbody
     (tr "Item" "Quantity" "Notes")))

  (define items (actor-inventory actor))
  (define items-list
    (for/list ([item items])
      (cond ((symbol? item)
             (tr
              (format "~a" item)
              (~v (item-quantity item))
              ""))
            ((ranged-weapon? item)
             (tr
              (item-name item)
              (~v (item-quantity item))
              (format "ammo left: ~a" (ranged-weapon-ammo-left item))))
            ((eq? (item-id item) 'bolt-cutters)
             (tr
              (item-name item)
              (~v (item-quantity item))
              ""))
            ((item? item)
             (tr
              (item-name item)
              (~v (item-quantity item))
              (if (not (null? (item-details item)))
                  (~v (item-details item))
                  "")))
            (else
             (tr
              (symbol->string item)
              (~v (item-quantity item))
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
