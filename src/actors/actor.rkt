#lang at-exp racket

(provide (all-defined-out))
(provide (all-from-out "0-types/actor.rkt"
                       "0-types/condition.rkt"
                       "0-types/pc-actor.rkt"
                       "0-types/status.rkt"))

(require "0-types/actor.rkt"
         "0-types/condition.rkt"
         "0-types/pc-actor.rkt"
         "0-types/status.rkt")

(require racket/lazy-require)

(require "../items/item.rkt"

         "../core/io.rkt"
         "../core/utils.rkt")


(lazy-require
 ["../state/state.rkt"
  (pc
   current-location
    display-combatant-info
   )]
 ["../world/world.rkt"
  (remove-actor-from-its-current-location!
   )]
 ["../locations/0-types/location.rkt"
  (add-feature-to-location!
   )])

(define (make-actor
         name
         type
         max-hp)
  (actor* name type max-hp max-hp
          ; attributes
          '() '() '() '() '()
          ; traits etc
          (make-hash) '() '() '() '() '()))

(define (actor-alive? actor)
  (> (actor-hp actor) 0))

(define (set-trait! actor trait-name trait-value)
  (hash-set! (actor-traits actor) trait-name trait-value))

(define (get-trait actor trait-name)
  (define result (hash-ref (actor-traits actor) trait-name 'not-found))
  (when (eq? result 'not-found)
    (dev-note (format
               "-- get-trait: trait [~a] not found on actor [~a]"
               trait-name
               (actor-name actor))))
  result)

(define (actor-add-status! actor status)
  (when (not (null? actor))
    (notice (format "~a: Status [~a] (~a turns) added" (actor-name actor) (status-type status) (status-lifetime status))))
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
  (if s
      (status-lifetime s)
      #f))

(define (decrement-actor-status-lifetimes! actor)
  (for ([status (actor-statuses actor)])
    (set-status-lifetime! status (- (status-lifetime status) 1)))
  (define new-statuses '())
  (for ([status (actor-statuses actor)])
    (if (positive? (status-lifetime status))
        (set! new-statuses (append-element new-statuses status))
        (notice
         (format "~a: Status [~a] removed"
                 (actor-name actor)
                 (status-type status)))))
  (set-actor-statuses! actor new-statuses))

; think:
; what if statuses are, by definition, something that's a fairly explicit list?
; and much of combat control is based on manipulating statuses?
(define (modify-actor-status-lifetime actor type modify-amount)
  (for ([status (actor-statuses actor)])
    (when (eq? (status-type status) type)
      (notice (format "~a: Status [~a] modified" (actor-name actor) (status-type status)))
      (set-status-lifetime! status (+ (status-lifetime status) modify-amount))))
  (define new-statuses '())
  (for ([status (actor-statuses actor)])
    (if (positive? (status-lifetime status))
        (set! new-statuses (append-element new-statuses status))
        (notice (format "~a: Status [~a] removed" (actor-name actor) (status-type status)))))
  (set-actor-statuses! actor new-statuses))

; yeah the way statuses currently work are a piece of shit
; but the idea of strength decreasing by one always at end of turn, as well as conditionally,
; it's a good idea
(define (actor-set-status! actor type value)
  (when (not (null? actor))
    (notice (format "~a: [~a]: ~a" (actor-name actor) type value)))

  (if (actor-has-status-of-type? actor type)
      (for ([status (actor-statuses actor)])
        (when (eq? (status-type status) type)
          (set-status-lifetime! status value)))
      (actor-add-status! actor (status type value))))

(define (actor-remove-status! actor status)
  (when (not (null? actor))
    (notice (format "~a: Status [~a] removed" (actor-name actor) (status-type status))))
  (set-actor-statuses! actor (remove status (actor-statuses actor))))

;;; CONDITIONS
(define (actor-add-condition! actor condition)
  (when (not (null? actor))
    (notice (format "~a: Condition [~a] added, details: [~a]" (actor-name actor) (condition-type condition) (condition-details condition))))
  (set-actor-conditions! actor (append-element (actor-conditions actor) condition)))

(define (actor-remove-condition! actor condition)
  (when (not (null? actor))
    (notice (format "~a: Condition [~a] removed" (actor-name actor) (condition-type condition))))
  (set-actor-conditions! actor (remove condition (actor-conditions actor))))

(define (actor-remove-condition-of-type! actor type)
  (when (not (null? actor))
    (notice (format "~a: Condition of type [~a] removed" (actor-name actor) type)))
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
        ((positive? modifier) (format "+~a" modifier))))


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
    (define mod-amount (exact-floor (* -0.5 damage)))
    (modify-actor-status-lifetime (pc) 'bound mod-amount)
    (notice (format "[Bound] strength Δ [~a], new strength [~a]"
                    mod-amount
                    (actor-lifetime-of-status-of-type? (pc) 'bound))))
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
       (format "~a is dead." (actor-name actor))
       (format "~a is dead. Cause of death: ~a"
               (actor-name actor)
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
   (tbody
    (tr
     (actor-name actor)
     "")
    (tr
     "hp:"
     (format "~a/~a" (actor-hp actor) (actor-max-hp actor))
     ))
   title))

(define (inflict-status! target status-type . status-strength)
  (match status-type
    ['blind
     (dev-note "todo: blind should be a condition, not a status")
     (p "The blindscraper swings its claw through an opening between Otava's arms. The claw tears diagonally across Otava's face, cutting its way through flesh, scraping bone.")
     (define roll (d 1 2))
     (wait-for-confirm)
     (case roll
       [(1)
        ; -> next generation: scars where there were wounds, then next: tattoos -> with both giving changes to the build - "the ghost that lived through" (it's often possible to name a reason)
        (p "A searing pain cuts through her left eye. Blood and intraocular fluid gush down her face.")]
       [(2)
        (p "A searing pain cuts through her eyes as her vision goes black.")])
     ]
    ['bound
     (actor-set-status! target status-type (car status-strength))]
    [else (notice (format "Status inflicted on ~a: [~a : ~a]" (actor-name target) status-type))]))

(define (inflict-condition! target a-condition)
  (match (condition-type a-condition)
    ['ankle-broken
     (if (actor-has-condition-of-type? target 'ankle-broken)
         (begin
           (actor-remove-condition-of-type! target 'ankle-broken)
           (actor-add-condition! target (condition 'both-ankles-broken "TODO"))
           )
         (actor-add-condition! target a-condition))
     ]
    ['bleeding
     (if (not (actor-has-condition-of-type? target 'bleeding))
         (actor-add-condition! target a-condition)
         (notice "Already bleeding."))

     ]
    [else (dev-note (format "todo: unknown condition ~a" condition))]))

(define (get-firearm actor)
  (define items (actor-inventory actor))
  (findf (λ (item) (ranged-weapon? item))
         items)
  )

(define (process-condition-on-end-turn owner condition)
  (case condition
    ['bleed
     (define bleed-damage-roll (d 1 6)) ; could give bonus from constitution here? say, 1d6?
     (cond ((= 1 bleed-damage-roll)
            (notice "Bleed check: 1d6 = 1: [1] => 1 dmg")
            (take-damage owner 1 'bleed)
            (display-combatant-info owner)
            )
           (else
            (notice (format "Bleed check: 1d6 = 1: [~a]" bleed-damage-roll))))]
    ['ankle-broken '()]
    [else (dev-note (format "process-condition-on-end-turn: unknown condition ~a" (condition-type condition)))])
  )


 (define (make-pc-actor
         name
         max-hp
         max-lp)
  (pc-actor*
   'pc name 'pc max-hp max-hp
   ; attributes
   '() '() '() '() '()
   ; traits etc
   (make-hash) '() '() '() '() '() max-lp max-lp 6 #t '() 0
   ; hunger
   200))

(define (pc-take-damage! actor damage damage-type)
  (when (< damage 0) (error "pc-take-damage: damage cannot be less than 0"))

  (cond ((not (positive? (actor-hp actor)))

         (define new-hp (- (actor-hp actor) damage))
         (set-actor-hp! actor new-hp)
         (notice (format "Taking damage, new HP : ~a]" new-hp))

         (define death-roll-dice (pc-actor-death-roll-dice actor))
         (define death-roll (d 1 death-roll-dice))
         (define result (+ death-roll
                           (actor-hp actor)))
         (notice (format "Death roll: 1d~a + HP = ~a – ~a = ~a"
                         death-roll-dice
                         death-roll
                         (abs (actor-hp actor)) ; slightly dirty: actor-hp *should* be non-positive
                         result))

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