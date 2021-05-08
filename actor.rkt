#lang racket

(provide (all-defined-out))

(require racket/serialize)

;; types
(serializable-struct
 actor
 (name
  [hp #:mutable]
  max-hp
  attack-skill
  attack-damage
  defense-number
  dexterity
  charisma
  [inventory #:mutable]
  statuses
  conditions
  [current-location #:mutable]))

(serializable-struct
 pc-actor
 (lp
  max-lp)
 #:super struct:actor)

;; operations
(define (add-item-to-inventory! actor item)
  (set-actor-inventory! actor
   (append (actor-inventory actor)
           (list item))))