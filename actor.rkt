#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(lazy-require
 ["situation.rkt"
  (clean-up-dead-actor!
   )])

(require racket/serialize)

(require "condition.rkt")
(require "io.rkt")
(require "status.rkt")
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

(define (actor-add-status! actor status)
  (when (not (null? actor))
    (displayln (string-append "[" (actor-name actor) ": Status [" (symbol->string (status-type status)) "] (" (number->string (status-lifetime status)) " turns) added]")))
  (set-actor-statuses! actor (append-element (actor-statuses actor) status)))

(define (actor-has-status-of-type? actor type)
  (if (memf (λ (status)
              (eq? (status-type status) type))
            (actor-statuses actor))
      #t
      #f))

(define (decrement-actor-status-lifetimes! actor)
  (for ([status (actor-statuses actor)])
    (set-status-lifetime! status (- (status-lifetime status) 1)))
  (define new-statuses '())
  (for ([status (actor-statuses actor)])
    (if (positive? (status-lifetime status))
        (set! new-statuses (append-element new-statuses status))
        (displayln
         (string-append
          "["
          (actor-name actor)
          ": Status ["
          (symbol->string (status-type status))
          "] removed]"))))
  (set-actor-statuses! actor new-statuses))

(define (actor-add-condition! actor condition)
  (when (not (null? actor))
    (displayln (string-append "[" (actor-name actor) ": Condition [" (symbol->string (condition-type condition)) "] added, details:]"))
    (displayln (condition-details condition)))
  (set-actor-conditions! actor (append-element (actor-conditions actor) condition)))

; TODO: Broken!
(define (actor-remove-condition! actor condition)
  (when (not (null? actor))
    (displayln (string-append "[" (actor-name actor) ": Condition [" (symbol->string (condition-type condition)) "] removed, details:]"))
    (displayln (condition-details condition)))
  (set-actor-conditions! actor (filter
                                (λ (other) (not (eq? (condition-type condition)
                                                     (condition-type other))))
                                (actor-conditions actor))))

(define (actor-remove-condition-of-type! actor type)
  (when (not (null? actor))
    (displayln (string-append "[" (actor-name actor) ": Condition of type [" (symbol->string type) "] removed]")))
  (set-actor-conditions! actor (filter
                                (λ (other) (not (eq? type
                                                     (condition-type other))))
                                (actor-conditions actor))))

(define (actor-has-condition-of-type? actor type)
  (if (memf (λ (condition)
              (eq? (condition-type condition) type))
            (actor-conditions actor))
      #t
      #f))




(serializable-struct
 pc-actor
 ([lp #:mutable]
  [max-lp #:mutable]
  [death-roll-dice #:mutable]
  [alive? #:mutable]
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
   (make-hash) '() '() '() '() max-lp max-lp 6 #t 0))

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


(define (pc-take-damage! actor damage)
  (displayln "pc-take-damage!")
  (when (< damage 0) (error "pc-take-damage: damage cannot be less than 0"))
  
  (cond ((not (positive? (actor-hp actor)))
         (displayln "DYING")
         
         (define new-hp (- (actor-hp actor) damage))
         (set-actor-hp! actor new-hp)
         (displayln (string-append "[Taking damage, new HP : "
                                   (number->string new-hp)
                                   "]"))
                  
         (define death-roll-dice (pc-actor-death-roll-dice actor))
         (define death-roll (d 1 death-roll-dice))
         (define result (+ death-roll
                           (actor-hp actor)))
         (displayln (string-append
                     "[Death roll: 1d"
                     (number->string death-roll-dice)
                     " + HP"
                     " = "
                     (number->string death-roll)
                     " - "
                     (number->string (abs (actor-hp actor))) ; slightly dirty: actor-hp *should* be non-positive
                     " = "
                     (number->string result)
                     "]"))

         (cond ((<= result 1)
                (begin
                  (displayln "TODO: (pc-die) or something")
                  'dead))
               (else
                'hit)
               ))
        
        (else
         (displayln "NOT DYING")
         (define new-hp (- (actor-hp actor) damage))
         (set-actor-hp! actor new-hp)
         'hit)))


(define (non-pc-take-damage! actor damage)
  (when (< damage 0) (error "non-pc-take-damage: damage cannot be less than 0"))
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

(define (take-damage actor damage)
  (if (pc-actor? actor)
      (pc-take-damage! actor damage)
      (non-pc-take-damage! actor damage)))

(define (kill actor cause-of-death)
  (set-actor-hp! actor 0)
  (displayln
   (string-append "["
                  (actor-name actor)
                  " is dead. Cause of death: "
                  (symbol->string cause-of-death)
                  "]"))
  
  (clean-up-dead-actor! actor))


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