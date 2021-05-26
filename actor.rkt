#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(lazy-require
 ["martaanvuo.rkt"
  (clean-up-dead-actor!
   )])

(require racket/serialize)

(require "utils.rkt")
(require "io.rkt")


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

(define (actor-alive? actor)
  (if (string? (actor-hp actor))
      #t
      (> (actor-hp actor) 0)))

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

(define (actor-add-status! actor status turns)
  (displayln (string-append "[" (actor-name actor) ": Status [" (symbol->string status) "] (" (number->string turns) " turns) added]"))
  (set-actor-statuses! actor (append-element (actor-statuses actor) (mcons status turns))))

(define (actor-has-status? actor status)
  (if (memf (λ (status_)
              (eq? (mcar status_) (status)))
            (actor-statuses actor))
      #t
      #f))

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

(define (get-attribute-modifier-for attribute)
  (cond ((= attribute 3) -3)
        ((<= 4  attribute  5) -2)
        ((<= 6  attribute  8) -1)
        ((<= 9  attribute 12)  0)
        ((<= 13 attribute 15)  1)
        ((<= 16 attribute 17)  2)
        ((= attribute 18) 3)))

(define (get-modifier-string modifier)
  (cond ((negative? modifier) (number->string modifier))
        ((= 0 modifier) (number->string modifier))
        ((positive? modifier) (string-append "+" (number->string modifier)))))


(define (take-damage actor damage)
  (when (< damage 0) (error "take-damage: damage cannot be less than 0"))
  (define new-hp (- (actor-hp actor) damage))
  (when (< new-hp 0) (set! new-hp 0))
  (set-actor-hp! actor new-hp)
  (define result
    (if (= 0 (actor-hp actor))
        'dead
        'hit))

  (when (eq? result 'dead)
    (clean-up-dead-actor! actor))
  
  result)


(define (add-item-to-inventory! actor item)
  (set-actor-inventory! actor
                        (append (actor-inventory actor)
                                (list item))))

(define (actor-status-card actor title)
  (info-card
   (list
    (list
     (string-append " " (actor-name actor) " ")
     "")
    (list
     " hp: "
     (string-append
      " "
      (number->string (actor-hp actor))
      "/"
      (number->string (actor-max-hp actor))
      " ")))
   title))