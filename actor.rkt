#lang racket

(provide (all-defined-out))

(require racket/serialize)

;; types
;; TODO: This is unwieldy, separate attributes etc to something like a (define checkables (make-hash))
;; because not everything will always have all of these
;; or, of course, look into composition
(serializable-struct
 actor
 (name
  [hp #:mutable]
  max-hp
  attack-skill
  attack-damage
  defense-number
  [constitution #:mutable]
  [strength #:mutable]
  [dexterity #:mutable]
  [charisma #:mutable]
  [intelligence #:mutable]
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