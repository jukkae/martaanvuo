#lang at-exp racket

(provide character-sheet
         display-inventory)

(require racket/lazy-require)

(require
  "../../0-engine/1-index/state.rkt"

  "../../0-engine/2-core/io.rkt"
  "../../0-engine/2-core/core.rkt"

  "../../0-engine/3-types/actor.rkt"
  "../../0-engine/3-types/condition.rkt"
  "../../0-engine/3-types/item.rkt"
  "../../0-engine/3-types/manipulator.rkt"
  "../../0-engine/3-types/modification.rkt"
  "../../0-engine/3-types/pc-actor.rkt"
  "../../0-engine/3-types/sense-organ.rkt"
  )

; Eventually, maybe lift this to the same "level" as numbered options. Then:
; Character sheet could be a treelike menu, where also letters are used.
; '-' is used to go back one level.
; Then, concrete actions with diegetic effects should return an action ultimately for (get-next-pc-action).
; Otherwise, return #t or whatever's appropriate for round-resolver
(define (character-sheet)
  (display-character-sheet)
  (when (not (empty? (pc-actor-modifications (pc))))
    (display-modifications))
  (when (not (empty? (pc-actor-sense-organs (pc))))
    (display-sense-organs))
  (when (not (empty? (pc-actor-manipulators (pc))))
    (display-manipulators))
  (display-inventory)
  ; TODO: until interaction-result is == go-back-to-game loop: select-interaction-target
  (define interaction-target (select-interaction-target))
  (when (not (null? interaction-target))
    (interaction-target))
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

  ;;; (define attributes-list '())
  ;;; (when (or (not (null? (actor-strength actor)))
  ;;;           (not (null? (actor-dexterity actor)))
  ;;;           (not (null? (actor-constitution actor)))
  ;;;           (not (null? (actor-intelligence actor)))
  ;;;           (not (null? (actor-charisma actor))))
  ;;;   (set! attributes-list
  ;;;         (append-element attributes-list
  ;;;                         (tr "" ""))))

  ;;; (when (not (null? (actor-strength actor)))
  ;;;   (set! attributes-list (append-element attributes-list
  ;;;                                         (tr
  ;;;                                          "strength"
  ;;;                                          (format "~a [~a]"
  ;;;                                                  (actor-strength actor)
  ;;;                                                  (get-modifier-string
  ;;;                                                   (get-attribute-modifier-for
  ;;;                                                    (actor-strength actor))))))))
  ;;; (when (not (null? (actor-dexterity actor)))
  ;;;   (set! attributes-list (append-element attributes-list
  ;;;                                         (tr
  ;;;                                          "dexterity"
  ;;;                                          (format "~a [~a]"
  ;;;                                                  (actor-dexterity actor)
  ;;;                                                  (get-modifier-string
  ;;;                                                   (get-attribute-modifier-for
  ;;;                                                    (actor-dexterity actor))))))))
  ;;; (when (not (null? (actor-constitution actor)))
  ;;;   (set! attributes-list (append-element attributes-list
  ;;;                                         (tr
  ;;;                                          "constitution"
  ;;;                                          (format "~a [~a]"
  ;;;                                                  (actor-constitution actor)
  ;;;                                                  (get-modifier-string
  ;;;                                                   (get-attribute-modifier-for
  ;;;                                                    (actor-constitution actor))))))))
  ;;; (when (not (null? (actor-intelligence actor)))
  ;;;   (set! attributes-list (append-element attributes-list
  ;;;                                         (tr
  ;;;                                          "intelligence"
  ;;;                                          (format "~a [~a]"
  ;;;                                                  (actor-intelligence actor)
  ;;;                                                  (get-modifier-string
  ;;;                                                   (get-attribute-modifier-for
  ;;;                                                    (actor-intelligence actor))))))))
  ;;; (when (not (null? (actor-charisma actor)))
  ;;;   (set! attributes-list (append-element attributes-list
  ;;;                                         (tr
  ;;;                                          "charisma"
  ;;;                                          (format "~a [~a]"
  ;;;                                                  (actor-charisma actor)
  ;;;                                                  (get-modifier-string
  ;;;                                                   (get-attribute-modifier-for
  ;;;                                                    (actor-charisma actor))))))))

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

  (set! sheet (append sheet #;attributes-list traits-list conditions-list hunger-list))

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

(define (select-item)
  (define items (actor-inventory (pc)))

  (prln (format "Select item [1-~a], anything else to cancel." (length items)))
  (br)

  (for ([item items]
        [i (in-naturals 1)])
    (if (= (item-quantity item) 1)
        (prln (format "[~a] ~a" i (item-name item)))
        (prln (format "[~a] ~a (~a)" i (item-name item) (item-quantity item))) ; TODO: pluralized
        )
    )
  (br)
  (define input (string->number (wait-for-input)))
  (define selected-item '())
  (cond ((and (number? input)
              (> input 0)
              (<= input (length items)))
         (define index (- input 1))
         (set! selected-item (list-ref items index))
         )
        (else '()#;(p "Nevermind.")))
  (when (not (null? selected-item))
    (dev-note (format "Item: ~a" selected-item))
    (when (eq? (item-id selected-item) 'lucky-charm-slot-machine)
      (cond [(eq? (item-details selected-item) 'active)
             (set-item-details! selected-item 'passive)
             (notice "Slot machine is now passive.")]
            [(eq? (item-details selected-item) 'passive)
             (set-item-details! selected-item 'active)
             (notice "Slot machine is now active.")]
            [else
             (notice (format "Unknown state: ~a" (item-details selected-item)))])
    ))
)

(define (display-modifications)
  (define actor (pc))

  (define header
    (tbody
     (tr "Modification" "Details")))

  (define modifications (pc-actor-modifications actor))
  (define modifications-list
    (for/list ([modification modifications])
      (tr
        (Modification-name modification)
        (~v (Modification-details modification)))
      ))

  (define sheet
    (append
     header
     modifications-list))

  (info-card
   sheet
   "Modifications"
   ))

(define (display-sense-organs)
  (define actor (pc))

  (define sense-organs (pc-actor-sense-organs actor))
  (define sense-organs-list
    (for/list ([sense-organ sense-organs])
      (tr
        (SenseOrgan-name sense-organ))))

  (info-card
   sense-organs-list
   "Sense organs"
   ))

(define (display-manipulators)
  (define actor (pc))

  (define manipulators (pc-actor-manipulators actor))
  (define manipulators-list
    (for/list ([manipulator manipulators])
      (tr
        (Manipulator-name manipulator))))

  (info-card
   manipulators-list
   "Manipulators"
   ))

(define (use-skill)
  (define skills '(switch-perspective))
  (prln (format "Select skill [1-~a], anything else to cancel." (length skills)))
  (br)

  (for ([skill skills]
        [i (in-naturals 1)])
    (prln (format "[~a] ~a" i skill))
    )
  (br)
  (define input (string->number (wait-for-input)))
  (define selected-skill '())
  (cond ((and (number? input)
              (> input 0)
              (<= input (length skills)))
         (define index (- input 1))
         (set! selected-skill (list-ref skills index))
         )
        (else '()#;(p "Nevermind.")))
  (when (not (null? selected-skill))
    (dev-note (format "Skill: ~a" selected-skill))
    (case selected-skill
     ['switch-perspective (toggle-flag 'perspective-switched)])
    )
  )

; TODO: These must be filtered w.r.t combat
(define (select-interaction-target)
  (define targets (list (cons "Select item" select-item)
                        (cons "Use skill" use-skill)))

  (prln (format "Select [1-~a], or anything else to go back to game." (length targets)))
  (br)

  (for ([target targets]
        [i (in-naturals 1)])
    (prln (format "[~a] ~a" i (car target)))
    )
  (br)
  (define input (string->number (wait-for-input)))
  (cond ((and (number? input)
              (> input 0)
              (<= input (length targets)))
         (define index (- input 1))
         (cdr (list-ref targets index))
         )
        (else '()#;(p "Nevermind.")))
  )