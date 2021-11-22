#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require 
  "character-sheet.rkt"

  "../actors/actor.rkt"
  "../actors/pc-actor.rkt"
  "../items/item.rkt"
  "../core/io.rkt"
  "../core/utils.rkt")

(lazy-require
 ["state/state.rkt"
  (pc
   )])

; PC hp <= 0 starts a "dying" counter of 1-2 rounds
; - this as an "upgrade" or talent or build thing or whatever
; - or a saving throw on any damage
(define (make-new-pc)
  (make-pc-actor
   "Otava"
   6
   4
   ))

(define (set-base-build!)
  (set-actor-strength! (pc) 7)
  (set-actor-dexterity! (pc) 10)
  (set-actor-constitution! (pc) 10)
  (set-actor-intelligence! (pc) 7)
  (set-actor-charisma! (pc) 7)
     
  (set-pc-actor-max-lp! (pc) 1)
  (set-pc-actor-lp! (pc) 1)

  (set-trait! (pc) "athletics-skill" 1)
  (set-trait! (pc) "melee-attack-skill" 3)
  (set-trait! (pc) "wrestle-attack-skill" -1)
  (set-trait! (pc) "defense" 1)
  (set-trait! (pc) "exploration-skill" 1)
  
  ; (add-item! 'knife #:amount 2 #:silent? #t)
  (add-item! 'ration #:amount 2 #:silent? #t)
  (add-item! 'fresh-berries #:amount 1 #:silent? #t)
  (add-item! 'vatruska #:amount 1 #:silent? #t)
  (add-item! 'bolt-cutters #:silent? #t)
  (add-item! 'revolver #:silent? #t)
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

    [else (error (string-append "set-build!: unknown build type " (symbol->string build)))])
  #;(inventory)
  #;(character-sheet)
  )

; TODO dispatching based on type should be done elsewhere,
; this should really be only concerned with adding an existing item
; but this works for now
(define (add-item!
         item
         #:amount [amount 1]
         #:title [title "Item added"]
         #:silent? [silent? #f])
  (define actor (pc))
  (cond ((symbol? item)
         (define new-item (make-item item #:amount amount))
         (add-item-to-inventory! actor new-item)
         (when (not silent?)
           (item-info-card new-item #:title title)))
        ((item? item)
         (add-item-to-inventory! actor item)
         (when (not silent?)
           (item-info-card item #:title title)))
        (else (error "Error: add-item! expects symbol or item"))))

(define (remove-item! id)
  (define items (actor-inventory (pc)))
  (findf (λ (inventory-item) (eq? (item-id inventory-item) id))
         items)
  (set-actor-inventory! (pc)
                        (filter (λ (inventory-item ) (not (eq? (item-id inventory-item) id)))
                                (actor-inventory (pc))
                                ))
  (dev-note "item removed, show info about removed/remaining items"))

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

(define (pc-gold-amount)
  (define items (actor-inventory (pc)))
  (define gold? (findf (λ (inventory-item) (eq? (item-id inventory-item) 'gold))
                       items))
  (if gold?
      (item-details gold?)
      0))

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
    (notice (format "Otava is ~a." hunger-string))
     
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
  ; TODO this is dirty hack, think about "aliveness" vs hp etc
  (case (pc-actor-alive? (pc))
   ['dead #f]
   [else
    (actor-alive? (pc)) 
    ]))

(define (kill-pc! cause-of-death)
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
      (displayln (string-append "[+" (number->string amount) " xp]"))
      (displayln (string-append "[+" (number->string amount) " xp: " (car reason) "]")))
  (set-pc-actor-xp! (pc)
                    (+ (pc-actor-xp (pc))
                       amount)))
