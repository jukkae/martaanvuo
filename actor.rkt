#lang racket

(provide (all-defined-out))

(require racket/serialize)

(require "utils.rkt")

(serializable-struct
 actor
 (name
  
  ; always numbers
  [hp #:mutable]
  [max-hp #:mutable]

  ; number or '()
  [strength #:mutable]
  [dexterity #:mutable]
  [constitution #:mutable]
  [intelligence #:mutable]
  [charisma #:mutable]

  ; hash of string-to-whatever-makes-sense
  [traits #:mutable]

  ; lists of symbols
  [statuses #:mutable]   ; (semi)temporary
  [conditions #:mutable] ; (semi)permanent
  
  [inventory #:mutable]
  [current-location #:mutable])
 #:constructor-name actor*)

(define (make-actor
         name
         max-hp)
  (actor* name max-hp max-hp
          ; attributes
          '() '() '() '() '()
          ; traits etc
          (make-hash) '() '() '() '()))

(define (set-trait! actor trait-name trait-value)
  (hash-set! (actor-traits actor) trait-name trait-value))

(define (get-trait actor trait-name)
  (define result (hash-ref (actor-traits actor) trait-name 'not-found))
  (when (eq? result 'not-found)
    (displayln (string-append
                "-- get-trait: trait "
                "\""
                trait-name
                "\""
                " not found on actor "
                (actor-name actor))))
  result)

(define (add-actor-status! actor status turns)
  (displayln (string-append "[" (actor-name actor) ": Status [" (symbol->string status) "] (" (number->string turns) " turns) added]"))
  (set-actor-statuses! actor (append-element (actor-statuses actor) (mcons status turns))))

(define (decrement-actor-status-lifetimes! actor)
  (for ([status (actor-statuses actor)])
    (set-mcdr! status (- (mcdr status) 1)))
  (set-actor-statuses! actor (filter
                              (λ (status) (positive? (mcdr status)))
                              (actor-statuses actor)))
  (displayln "DECREMENTING"))

(serializable-struct
 pc-actor
 ([lp #:mutable]
  [max-lp #:mutable]
  [xp #:mutable])
 #:super struct:actor
 #:constructor-name pc-actor*)

(define (make-pc-actor
         name
         max-hp
         max-lp)
  (pc-actor*
   name max-hp max-hp
   ; attributes
   '() '() '() '() '()
   ; traits etc
   (make-hash) '() '() '() '() max-lp max-lp 0))

;; operations
(define (add-item-to-inventory! actor item)
  (set-actor-inventory! actor
                        (append (actor-inventory actor)
                                (list item))))