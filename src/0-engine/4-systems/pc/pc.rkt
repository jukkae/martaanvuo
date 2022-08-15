#lang at-exp racket

(provide (all-defined-out))

(require
  "../actors/actor.rkt"
  "../items/item.rkt"

  "../../1-index/state.rkt"

  "../../2-core/io.rkt"
  "../../2-core/core.rkt"

  "../../3-types/actor.rkt"
  "../../3-types/item.rkt"
  "../../3-types/modification.rkt"
  "../../3-types/sense-organ.rkt"
  "../../3-types/manipulator.rkt"
  "../../3-types/pc-actor.rkt"
  )

; PC hp <= 0 starts a "dying" counter of 1-2 rounds
; - this as an "upgrade" or talent or build thing or whatever
; - or a saving throw on any damage
(define (make-new-pc)
  (make-pc-actor
   "Otava"
   #:max-hp 6
   #:max-lp 4
   #:size 'large
   #:sense-organs (list
    (SenseOrgan 'basic-homeostasis "basic homeostasis")
    (SenseOrgan 'nose "nose")
    ;(SenseOrgan 'eyes "eyes")
    (SenseOrgan 'ears "ears")
    (SenseOrgan 'echolocation "echolocation")
    ;(SenseOrgan 'touch-sensitive-skin "touch-sensitive skin")
    )
   #:manipulators (list
    (Manipulator 'human-hands "hands at end of arms")
    (Manipulator 'legs "feet at end of legs")
    (Manipulator 'mouth "speech-capable mouth")
    ; (Manipulator 'nimble-hands "nimble hands")
    ; (Manipulator 'legs "legs")
    ; (Manipulator 'wings "wings")
    ; (Manipulator 'prehensile-feet "prehensile feet")
    ; (Manipulator 'sharp-incisors "sharp incisors")
    )
   ))

(define (set-base-build!)
  (set-actor-strength! (pc) 7)
  (set-actor-dexterity! (pc) 10)
  (set-actor-constitution! (pc) 10)
  (set-actor-intelligence! (pc) 7)
  (set-actor-charisma! (pc) 7)

  (set-pc-actor-max-lp! (pc) 1)
  (set-pc-actor-lp! (pc) 1)

  ; (set-trait! (pc) "athletics-skill" 1)
  (set-trait! (pc) "melee-attack-skill" 3)
  ; (set-trait! (pc) "wrestle-attack-skill" -1)
  (set-trait! (pc) "defense" 1)
  ; (set-trait! (pc) "exploration-skill" 1)

  ; (add-item! 'knife #:amount 2 #:silent? #t)
  (add-item! 'ration #:amount 2 #:silent? #t)
  (add-item! 'bolt-cutters #:silent? #t)
  (add-item! 'lucky-charm-slot-machine #:silent? #t)
  )

(define (set-build! build)
  (set-base-build!)
  (case build
    ['rope
     (add-item! 'rope #:silent? #t)
     ]

    ['flashlight
     (add-item! 'flashlight #:amount 1 #:silent? #t)
     ]

    [else (error (format "set-build!: unknown build type ~a" build))])
  #;(inventory)
  #;(character-sheet)
  )

; (: -> Modification '())
(define (add-modification! modification)
  (define old-modifications (pc-actor-modifications (pc)))
  (set-pc-actor-modifications! (pc) (append-element old-modifications modification))
  (modification-info-card modification #:title "Modification installed"))

; (: -> modification-id '())
(define (remove-modification! id)
  (define modifications (pc-actor-modifications (pc)))
  (define it
    (findf (λ (modification) (eq? (Modification-id modification) id))
           modifications))
  (cond (it
         (set-pc-actor-modifications!
            (pc)
            (filter (λ (modification) (not (eq? (Modification-id modification) id)))
                    (actor-inventory (pc))
                    ))
         (dev-note "modification removed, show info about removed/remaining modifications"))
        ))

; (: -> (U Item Symbol) Natural String Boolean '())
(define (add-item!
         item
         #:amount [amount 1]
         #:title [title "Item added"]
         #:silent? [silent? #f])
  (define actor (pc))
  (when (symbol? item) (set! item (make-item item #:amount amount)))
  (add-item-to-inventory! actor item)
  (when (not silent?)
    (item-info-card item #:title title)))

(define (remove-item! id #:quantity-to-remove [quantity-to-remove 1])
  (define items (actor-inventory (pc)))
  (define it
    (findf (λ (inventory-item) (eq? (item-id inventory-item) id))
           items))
  (cond (it
         (cond [(eq? quantity-to-remove 'all)
                (set-actor-inventory! (pc)
                                      (filter (λ (inventory-item ) (not (eq? (item-id inventory-item) id)))
                                              (actor-inventory (pc))
                                              ))]
               [else
                (if (> (item-quantity it) 1)
                    (set-item-quantity! it (- (item-quantity it) 1))
                    (set-actor-inventory! (pc)
                                          (filter (λ (inventory-item ) (not (eq? (item-id inventory-item) id)))
                                                  (actor-inventory (pc))
                                                  ))
                    )
                ]
               )
         #;(dev-note "item removed, show info about removed/remaining items") ; TODO: re-enable and implement this
         )
        ))

(define (add-ammo! amount)
  (define items (actor-inventory (pc)))
  (define revolver (findf (λ (inventory-item) (eq? (item-id inventory-item) 'revolver))
                          items))
  (increase-ammo! revolver))

(define (consume-ammo! amount)
  (define items (actor-inventory (pc)))
  (define revolver (findf (λ (inventory-item) (eq? (item-id inventory-item) 'revolver))
                          items))
  (cond (revolver
         (set-ranged-weapon-ammo-left! revolver (sub1 (ranged-weapon-ammo-left revolver))))))

(define (pc-has-item? id)
  (define items (actor-inventory (pc)))
  (findf (λ (inventory-item) (eq? (item-id inventory-item) id))
         items))

(define (pc-has-sense-organ? id)
  (define sense-organs (pc-actor-sense-organs (pc)))
  (findf (λ (sense-organ) (eq? (SenseOrgan-id sense-organ) id))
         sense-organs))

; (: -> SenseOrgan '())
(define (add-sense-organ! sense-organ)
  (define old-sense-organs (pc-actor-sense-organs (pc)))
  (set-pc-actor-sense-organs! (pc) (append-element old-sense-organs sense-organ))
  (notice (format "Sense organ added: ~a" (SenseOrgan-name sense-organ))))

; (: -> SenseOrgan-id '())
(define (remove-sense-organ! id)
  (define sense-organs (pc-actor-sense-organs (pc)))
  (define it
    (findf (λ (sense-organ) (eq? (SenseOrgan-id sense-organ) id))
           sense-organs))
  (cond (it
         (set-pc-actor-sense-organs!
            (pc)
            (filter (λ (sense-organ) (not (eq? (SenseOrgan-id sense-organ) id)))
                    (pc-actor-sense-organs (pc))
                    ))
         (dev-note "sense-organ removed, show info about removed/remaining sense-organs"))
        ))

(define (pc-has-manipulator? id)
  (define manipulators (pc-actor-manipulators (pc)))
  (findf (λ (manipulator) (eq? (Manipulator-id manipulator) id))
         manipulators))

(define (pc-gold-amount)
  (define items (actor-inventory (pc)))
  (define gold? (findf (λ (inventory-item) (eq? (item-id inventory-item) 'gold))
                       items))
  (if gold?
      (item-quantity gold?)
      0))

(define (pc-has-money amount)
  (>= (pc-gold-amount) amount))

(define (decrease-pc-money! quantity)
  (define items (actor-inventory (pc)))
  (define gold? (findf (λ (inventory-item) (eq? (item-id inventory-item) 'gold))
                       items))
  (cond [gold?
         (set-item-quantity! gold? (- (item-quantity gold?) quantity))])
  (notice (format "Gold decreased by ~a, new quantity: ~a" quantity (item-quantity gold?)))
)

; TODO: combine
(define (increase-pc-money! quantity)
  (define items (actor-inventory (pc)))
  (define gold? (findf (λ (inventory-item) (eq? (item-id inventory-item) 'gold))
                       items))
  (cond [(not gold?)
         (add-item! 'gold #:amount quantity)
         (notice (format "Otava now has ~a gold" quantity))]
        [else
         (set-item-quantity! gold? (+ (item-quantity gold?) quantity))
         (notice (format "Gold increased by ~a, new quantity: ~a" quantity (item-quantity gold?)))])
)

(define (pc-has-ammo-left?)
  (define items (actor-inventory (pc)))
  (define revolver (findf (λ (inventory-item) (eq? (item-id inventory-item) 'revolver))
                          items))
  (cond (revolver
         (define ammo-left (ranged-weapon-ammo-left revolver))
         (positive? ammo-left))
        (else #f)))

(define (pc-hungry?)
  (or (eq? (pc-hunger-level) 'hungry)
      (eq? (pc-hunger-level) 'very-hungry)
      (eq? (pc-hunger-level) 'starving)))

(define hunger-level-hungry 400)
(define hunger-level-very-hungry 800)
(define hunger-level-starving 1600)

; a day is 600 ticks -> if you eat always when you get hungry, once a day is not enough
(define (decrease-pc-hunger-level levels)
  (let ([current-hunger (pc-hunger-level)])
    (case current-hunger
      ['satiated (set-pc-actor-hunger! (pc) 0)]
      ['not-hungry (set-pc-actor-hunger! (pc) 0)]
      ['hungry
       (case levels
         [(0) (set-pc-actor-hunger! (pc) hunger-level-hungry)]
         [(1) (set-pc-actor-hunger! (pc) 0)]
         [(2) (set-pc-actor-hunger! (pc) 0)])]
      ['very-hungry
       (case levels
         [(0) (set-pc-actor-hunger! (pc) hunger-level-very-hungry)]
         [(1) (set-pc-actor-hunger! (pc) hunger-level-hungry)]
         [(2) (set-pc-actor-hunger! (pc) 0)])]
      ['starving
       (case levels
         [(0) (set-pc-actor-hunger! (pc) hunger-level-starving)]
         [(1) (set-pc-actor-hunger! (pc) hunger-level-very-hungry)]
         [(2) (set-pc-actor-hunger! (pc) hunger-level-hungry)])])

    (when (and (or (eq? current-hunger 'hungry)
                   (eq? current-hunger 'very-hungry)
                   (eq? current-hunger 'starving))
               (= levels 2))
      (p "Damn that tastes good.")
      (award-xp! 50)
      )

    (define hunger-string
      (case (pc-hunger-level)
        ['satiated "now satiated"]
        ['not-hungry "not hungry anymore"]
        ['hungry "less hungry"]
        ['very-hungry "still very hungry"]
        ['starving "still starving"])
      )
    (notice (format "Otava is now ~a." hunger-string))
    )
  )

(define (pc-hunger-level)
  (cond
    ((<= (pc-actor-hunger (pc)) 50)
     'satiated)
    ((< (pc-actor-hunger (pc)) hunger-level-hungry)
     'not-hungry)
    ((< (pc-actor-hunger (pc)) hunger-level-very-hungry)
     'hungry)
    ((< (pc-actor-hunger (pc)) hunger-level-starving)
     'very-hungry)
    (else
     'starving)
    ))

(define (pc-is-alive?)
  (case (pc-actor-alive? (pc))
    ['dead #f]
    [else
     (actor-alive? (pc))
     ]))

(define (kill-pc! cause-of-death)
  (clear-current-fragment!)
  (narrate-pc-death cause-of-death)
  (kill (pc) cause-of-death))

(define (narrate-pc-death cause-of-death)
  (case cause-of-death
    ['fell-to-death
     (p "Otava's mangled body hangs limp against the rock wall, a feast for carrioneaters, torn apart, sustenance for new generations, possessions strewn in the underbrush.")])
  )

(provide award-xp!)
(define (award-xp! amount . reason)
  (if (null? reason)
      (notice (format "+~a xp" amount))
      (notice (format "+~a xp ~a" amount reason)))
  (set-pc-actor-xp! (pc)
                    (+ (pc-actor-xp (pc))
                       amount)))
