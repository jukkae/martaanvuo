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
  [max-hp #:mutable]
  traits
  statuses
  conditions
  [inventory #:mutable]
  [current-location #:mutable])
 #:constructor-name actor*)

(define (make-actor
         name
         max-hp)
  (actor* name max-hp max-hp (make-hash) '() '() '() '()))

;;some common traits
#;(list attack-skill
      attack-damage
      defense-number
      [constitution #:mutable]
      [strength #:mutable]
      [dexterity #:mutable]
      [charisma #:mutable]
      [intelligence #:mutable]
      )

(define (set-trait! actor trait-name trait-value)
  (hash-set! (actor-traits actor) trait-name trait-value))
(define (get-trait actor trait-name)
  (hash-ref (actor-traits actor) trait-name))

(serializable-struct
 pc-actor
 (lp
  max-lp)
 #:super struct:actor
 #:constructor-name pc-actor*)

(define (make-pc-actor
         name
         max-hp
         max-lp)
  (pc-actor* name max-hp max-hp (make-hash) '() '() '() '() max-lp max-lp))

;; operations
(define (add-item-to-inventory! actor item)
  (set-actor-inventory! actor
                        (append (actor-inventory actor)
                                (list item))))