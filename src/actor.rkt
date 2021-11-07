#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(lazy-require
 ["state/state.rkt"
  (pc
   current-location
   )]
 ["world.rkt"
  (remove-actor-from-its-current-location!
   )]
 ["locations/location.rkt"
  (add-feature-to-location!
   )])


(require racket/serialize)

(require "condition.rkt")
(require "io.rkt")
(require "item.rkt")
(require "stance.rkt")
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
  [location #:mutable]
  [stance #:mutable]) ; only NPCs
 #:constructor-name actor*)

(serializable-struct
 pc-actor
 actor
 ([lp #:mutable]
  [max-lp #:mutable]
  [death-roll-dice #:mutable]
  [alive? #:mutable]
  [cause-of-death #:mutable]
  [xp #:mutable]
  [hunger #:mutable])
 #:constructor-name pc-actor*)

(define (make-actor
         name
         max-hp)
  (actor* name max-hp max-hp
          ; attributes
          '() '() '() '() '()
          ; traits etc
          (make-hash) '() '() '() '() '()))

(define (make-pc-actor
         name
         max-hp
         max-lp)
  (pc-actor*
   name max-hp max-hp
   ; attributes
   '() '() '() '() '()
   ; traits etc
   (make-hash) '() '() '() '() '() max-lp max-lp 6 #t '() 0 0))

(define (actor-alive? actor)
  (> (actor-hp actor) 0))


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

(define (actor-lifetime-of-status-of-type? actor type)
  (define s (findf (λ (status)
                     (eq? (status-type status) type))
                   (actor-statuses actor)))
  (status-lifetime s))

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

; think:
; what if statuses are, by definition, something that's a fairly explicit list?
; and much of combat control is based on manipulating statuses?
(define (modify-actor-status-lifetime actor type modify-amount)
  (for ([status (actor-statuses actor)])
    (when (eq? (status-type status) type)
      (displayln (string-append "[" (actor-name actor) ": Status [" (symbol->string (status-type status)) "] modified." "]"))
      (set-status-lifetime! status (+ (status-lifetime status) modify-amount))))
  
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

; yeah the way statuses currently work are a piece of shit
; but the idea of strength decreasing by one always at end of turn, as well as conditionally,
; it's a good idea
(define (actor-set-status! actor type value)
  (when (not (null? actor))
    (displayln (string-append "[" (actor-name actor) ": Setting status [" (symbol->string type) "] strength to (" (number->string value) " turns)]")))

  (if (actor-has-status-of-type? actor type)
      (for ([status (actor-statuses actor)])
        (when (eq? (status-type status) type)
          (set-status-lifetime! status value)))
      (actor-add-status! actor (status type value))))






;;; CONDITIONS
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


(define (pc-take-damage! actor damage damage-type)
  (when (< damage 0) (error "pc-take-damage: damage cannot be less than 0"))
  
  (cond ((not (positive? (actor-hp actor)))

         
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

         (define cause-of-death damage-type)

         (cond ((<= result 1)
                (begin
                  (kill actor cause-of-death)
                  'dead))
               (else
                'hit)
               ))
        
        (else
         (define new-hp (- (actor-hp actor) damage))
         (when (not (positive? new-hp))
           (displayln "[Otava is dying.]")
           (wait-for-confirm))
              
         (set-actor-hp! actor new-hp)
         'hit)))


(define (non-pc-take-damage! actor damage damage-type)
  (when (< damage 0) (error "non-pc-take-damage: damage cannot be less than 0"))
  (define new-hp (- (actor-hp actor) damage))
  (when (< new-hp 0) (set! new-hp 0))
  (set-actor-hp! actor new-hp)
  (define result
    (if (= 0 (actor-hp actor))
        'dead
        'hit))

  (when (eq? result 'dead)
    (kill actor))

  ; TODO this belongs to grabberkin.rkt
  ; -> subtype grabberkin-actor from actor?
  (when (equal? (actor-name actor)
                "Grabberkin")
    (modify-actor-status-lifetime (pc) 'bound (* -1 damage))
    (displayln (string-append "decrease bound status strength by "
                              (number->string (* -1 damage)))))
    
  
  result)

(define (take-damage actor damage damage-type)
  (if (pc-actor? actor)
      (pc-take-damage! actor damage damage-type)
      (non-pc-take-damage! actor damage damage-type)))

(define (describe-cause-of-death cause-of-death)
  (case cause-of-death
    ['fell-to-death "Concussion and internal bleeding."]
    [else (symbol->string cause-of-death)]))

(define (kill actor . cause-of-death)
  (set-actor-hp! actor 0)
  (notice
   (if (null? cause-of-death)
       (string-append (actor-name actor)
                      " is dead.")
       (string-append (actor-name actor)
                      " is dead. Cause of death: "
                      (cond ((symbol? cause-of-death)
                             (describe-cause-of-death cause-of-death))
                            ((string? cause-of-death)
                             cause-of-death)
                            ((symbol? (car cause-of-death))
                             (describe-cause-of-death (car cause-of-death)))
                            ((string? (car cause-of-death))
                             (car cause-of-death))
                            (else "NA"))
                      )))
  
  (cond ((pc-actor? actor)
         (set-pc-actor-alive?! actor 'dead)
         (set-pc-actor-cause-of-death! actor cause-of-death)
         )
        (else
         (remove-actor-from-its-current-location! actor)
         (define corpse (cons 'corpse "Corpse (TODO)"))
         (add-feature-to-location! (current-location) corpse))))


(define (add-item-to-inventory! actor item)
  
  (set-actor-inventory! actor
                        (append (actor-inventory actor)
                                (list item))))

(define (actor-has-item? actor item)
  (define inventory (actor-inventory actor))
  (findf (λ (inventory-item) (eq? (item-id inventory-item) item))
         inventory))


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

(define (inflict-status! target status)
  (match status
    ['blind
     (displayln "todo: blind should be a condition, not a status")
     (p "The Blindscraper swings its claw through an opening between Otava's arms. The claw tears diagonally across Otava's face, cutting its way through flesh, scraping bone.")
     (define roll (d 1 2))
     (wait-for-confirm)
     (case roll
       [(1)
        ; -> next generation: scars where there were wounds, then next: tattoos -> with both giving changes to the build - "the ghost that lived through" (it's often possible to name a reason)
        (p "A searing pain cuts through her left eye. Blood and intraocular fluid gush down her face.")]
       [(2)
        (p "A searing pain cuts through her eyes as her vision turns to black.")])
     ]
    ['bound
     (actor-set-status! target (status-type status) (status-lifetime status))
     ]
    [else (p "todo: unknown status")]))

(define (inflict-condition! target cond)
  (match (condition-type cond)
    ['ankle-broken
     (if (actor-has-condition-of-type? target 'ankle-broken)
         (begin
           (actor-remove-condition-of-type! target 'ankle-broken)
           (actor-add-condition! target (condition 'both-ankles-broken "TODO"))
           )
         (actor-add-condition! target cond))
     ]
    ['bleeding
     (if (not (actor-has-condition-of-type? target 'bleeding))
         (actor-add-condition! target cond)
         (displayln "Already bleeding."))
     
     ]
    [else (p "todo: unknown condition")]))

(define (get-firearm actor)
  (define items (actor-inventory actor))
  (findf (λ (item) (ranged-weapon? item))
         items)
  )