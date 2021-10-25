#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "actor.rkt")
(require "character-sheet.rkt")
(require "io.rkt")
(require "item.rkt")
(require "utils.rkt")

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
  
  (add-item! 'knife #:amount 2 #:silent? #t)
  (add-item! 'ration #:amount 2 #:silent? #t)
  (add-item! 'bolt-cutters #:silent? #t)
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
  (> (pc-actor-hunger (pc)) 500))

(define (pc-hunger-level)
  (cond
    ((<= (pc-actor-hunger (pc)) 0)
     'satiated)
    ((<= (pc-actor-hunger (pc)) 500)
     'not-hungry)
    ((<= (pc-actor-hunger (pc)) 1300)
     'hungry)
    ((<= (pc-actor-hunger (pc)) 2100)
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
  (kill (pc) cause-of-death))

(provide award-xp!)
(define (award-xp! amount . reason)
  (if (null? reason)
      (displayln (string-append "[+" (number->string amount) " xp]"))
      (displayln (string-append "[+" (number->string amount) " xp: " (car reason) "]")))
  (set-pc-actor-xp! (pc)
                    (+ (pc-actor-xp (pc))
                       amount)))
