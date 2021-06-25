#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "actor.rkt")
(require "character-sheet.rkt")
(require "io.rkt")
(require "item.rkt")
;(require "situation.rkt")
(require "utils.rkt")

(lazy-require
 ["situation.rkt"
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

(define (set-build! build)
  (case build
    
    ['gun
     (set-actor-strength! (pc) 7)
     (set-actor-dexterity! (pc) 10)
     (set-actor-constitution! (pc) 7)
     (set-actor-intelligence! (pc) 7)
     (set-actor-charisma! (pc) 10)
     
     (set-pc-actor-max-lp! (pc) 1)
     (set-pc-actor-lp! (pc) 1)

     (set-trait! (pc) "athletics-skill" 0)
     (set-trait! (pc) "melee-attack-skill" 0)
     (set-trait! (pc) "wrestle-attack-skill" 1)
     (set-trait! (pc) "defense" 1)
     (add-item! 'revolver)
     ]
    
    ['bruiser
     (set-actor-strength! (pc) 10)
     (set-actor-dexterity! (pc) 10)
     (set-actor-constitution! (pc) 10)
     (set-actor-intelligence! (pc) 7)
     (set-actor-charisma! (pc) 7)
     
     (set-pc-actor-max-lp! (pc) 0)
     (set-pc-actor-lp! (pc) 0)

     (set-trait! (pc) "athletics-skill" 1)
     (set-trait! (pc) "melee-attack-skill" 3)
     (set-trait! (pc) "wrestle-attack-skill" -1)
     (set-trait! (pc) "defense" 1)]

    [else (error (string-append "set-build!: unknown build type )" (symbol->string build)))])

  
  (set-trait! (pc) "exploration-skill" 1)
  (character-sheet)
  (print-inventory)
  )

; TODO dispatching based on type should be done elsewhere,
; this should really be only concerned with adding an existing item
; but this works for now
(define (add-item! item)
  (define actor (pc))
  (cond ((symbol? item)
         (define new-item (make-item item))
         (add-item-to-inventory! actor new-item))
        ((item? item)
         (add-item-to-inventory! actor item))
        (else (error "Unknown item type in add-item!"))))

(define (pc-has-item? id)
  (define items (actor-inventory (pc)))
  (findf (λ (inventory-item) (eq? (item-id inventory-item) id))
         items))

(define (print-inventory)
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
            ((item? item)
             (list
              (string-append " " (item-name item) " ")
              (string-append " " (~v (item-details item)) " ")))
            (else (list
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
