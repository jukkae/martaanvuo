#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "actor.rkt")
(require "character-sheet.rkt")
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
  
  ; for desperate build, also set a time limit (or whatever other complication)

  (define starting-inventory
    (list
     (list 'bolt-cutters (list 'melee-weapon 'tool))))

  
  (case build
    
    ['desperate
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
     (set-trait! (pc) "defense" 1)]
    
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

  (set-actor-inventory! (pc) starting-inventory)
  (character-sheet)
  )