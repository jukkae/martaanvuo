#lang at-exp racket

(provide (all-defined-out))
(provide (all-from-out
  "conditions.rkt"
  "statuses.rkt"))

(require "conditions.rkt"
         "statuses.rkt")

(require racket/lazy-require)

(require
  "../items/item.rkt"

  "../../1-index/state.rkt"

  "../../2-core/io.rkt"
  "../../2-core/core.rkt"

  "../../3-types/condition.rkt"
  "../../3-types/actor.rkt"
  "../../3-types/item.rkt"
  "../../3-types/name.rkt"
  "../../3-types/pc-actor.rkt"

  "../../3-types/stance.rkt"
  )

(lazy-require ["../world/world.rkt"
  (remove-actor-from-its-current-location!
   move-actor-to-location!
   )])
(lazy-require ["../world/time.rkt"
  (timestamp
   )])
(lazy-require ["../../3-types/location.rkt"
  (add-feature-to-location!
   add-item-to-location!
   )])
(lazy-require ["../../7-state/state.rkt"
  (current-times-species-encountered
   current-times-species-encountered++
   )])
(lazy-require ["../../7-state/mutators.rkt"
  (get-enemies-at-range
   )])
; TODO: refactor
(lazy-require ["../../../1-content/enemies/grabberkin.rkt"
  (make-grabberkin
   )])
(lazy-require ["../../../1-content/enemies/blindscraper.rkt"
  (make-blindscraper
   )])
(lazy-require ["../../../1-content/enemies/human-fighter.rkt"
  (make-human-fighter
   )])
(lazy-require ["../../../1-content/enemies/voidfloater.rkt"
  (make-voidfloater
   )])
(lazy-require ["../../../1-content/enemies/limbtearer.rkt"
  (make-limbtearer
   )])

(define (make-actor
         name
         type
         #:max-hp max-hp
         #:size size
         )
  (define id (get-next-numeric-actor-id))
  (actor* id name type max-hp max-hp size
          ; attributes
          '() '() '() '() '()
          ; traits etc
          (make-hash) '() '() '() '() '()))

(define (actor-alive? actor)
  (> (actor-hp actor) 0))

(define (set-trait! actor trait-name trait-value)
  (hash-set! (actor-traits actor) trait-name trait-value))

(define (get-trait actor trait-name)
  (cond (actor
         (define result (hash-ref (actor-traits actor) trait-name 'not-found))
         (when (equal? result 'not-found)
           (dev-note (format
                      "-- get-trait: trait [~a] not found on actor [~a]"
                      trait-name
                      (actor-name actor))))
         result)
        (else 'not-found)
        ))


(define (non-pc-take-damage! actor damage damage-type)
  (when (< damage 0)
    (notice (format "Negative damage ~a not allowed." damage))
    (set! damage 0))
  (define new-hp (- (actor-hp actor) damage))
  (when (< new-hp 0) (set! new-hp 0))
  (set-actor-hp! actor new-hp)
  (define result
    (if (= 0 (actor-hp actor))
        'dead
        'hit))

  (when (equal? result 'dead)
    (kill actor))

  ; should be moved to grabberkin.rkt
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

(define (kill actor [cause-of-death '()])
  (set-actor-hp! actor 0)
  (notice
   (if (null? cause-of-death)
       (format "~a is dead." (actor-name actor))
       (format "~a is dead. Cause of death: ~a"
               (actor-name actor)
               (cond [(symbol? cause-of-death)
                      (describe-cause-of-death cause-of-death)]
                     [(string? cause-of-death)
                      cause-of-death]
                     [else "NA"]
               ))))

  (cond ((pc-actor? actor)
         (set-pc-actor-alive?! actor #f)
         (set-pc-actor-cause-of-death! actor cause-of-death)
         )
        (else
         (remove-actor-from-its-current-location! actor)
         (add-item-to-location! (current-location) (make-corpse actor)))))

(define (make-corpse actor)
  (define corpse-name
    (Name
     (string-append (actor-name actor) " corpse")
     "a"
     "'s"
     (string-append (actor-name actor) " corpses")))
  (define i (new-item
   corpse-name
   #:id (string->symbol (string-append (string-downcase (actor-name actor)) "-corpse"))))
  (notice (format "A ~a dropped." (string-append (actor-name actor) " corpse")))
  i)

(define (inventory-contains-item-id inventory id)
  (findf (λ (i) (equal? (item-id i) id))
         inventory)
  )

(define (add-item-to-inventory! actor item)
  (cond ((inventory-contains-item-id (actor-inventory actor) (item-id item))
         (define i (inventory-contains-item-id (actor-inventory actor) (item-id item)))
         (set-item-quantity! i (+ (item-quantity i) (item-quantity item))))
        (else
         (set-actor-inventory! actor
                               (append (actor-inventory actor)
                                       (list item))))))

(define (actor-has-item? actor item)
  (define id (cond ((symbol? item) item)
                   (else (item-id item))))
  (define inventory (actor-inventory actor))
  (findf (λ (inventory-item)
           (cond ((symbol? inventory-item)
                  (equal? id inventory-item))
                 (else
                  (equal? (item-id inventory-item) id))))
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
    ['bound
     (actor-set-status! target status-type (car status-strength))]
    [else (notice (format "Status inflicted on ~a: [~a : ~a]" (actor-name target) status-type))]))

(define (inflict-condition! target a-condition)
  (match (condition-type a-condition)
    ['ankle-broken
     (if (actor-has-condition-of-type? target 'ankle-broken)
         (begin
           (actor-remove-condition-of-type! target 'ankle-broken)
           #;(actor-add-condition! target (condition 'both-ankles-broken "Can only crawl."))
           )
         (actor-add-condition! target a-condition))
     ]
    ['bleeding
     (if (not (actor-has-condition-of-type? target 'bleeding))
         (actor-add-condition! target a-condition)
         (notice "Already bleeding."))
     ]
    ['windpipe-broken
     (if (not (actor-has-condition-of-type? target 'windpipe-broken))
         (actor-add-condition! target a-condition)
         (notice "Windpipe already broken."))
     ]
    ['one-eye-blind '()]
    ['both-eyes-blind '()]
    ['envenomed
     (actor-add-condition! target a-condition)]
    ['dislocated-shoulder (actor-add-condition! target a-condition)]
    ['arm-missing
     (when (actor-has-condition-of-type? target 'dislocated-shoulder)
       (actor-remove-condition-of-type! target 'dislocated-shoulder))
     (actor-add-condition! target a-condition)]
    ['bleeding-profusely
     (when (actor-has-condition-of-type? target 'bleeding)
       (actor-remove-condition-of-type! target 'bleeding))
     (when (not (actor-has-condition-of-type? target 'bleeding-profusely))
       (actor-add-condition! target a-condition))
     ]
    [else (dev-note (format "Unknown condition ~a" a-condition))]
    ))

; TODO: this belongs to content
(define (actor-process-condition-tick actor condition)
  (case (condition-type condition)
    ['envenomed
     (when (= (condition-age condition) 5)
       (notice (format "[~a] [~a] Envenomation is now acute." (actor-name actor) (current-elapsed-time)))
       (set-Illness-state! condition 'acute))
     (when (= (condition-age condition) 100)
       (notice (format "[~a] [~a] Envenomation is now postacute." (actor-name actor) (current-elapsed-time)))
       (set-Illness-state! condition 'postacute))
     (when (= (condition-age condition) 700)
       (notice (format "[~a] [~a] Envenomation has now healed." (actor-name actor) (current-elapsed-time)))
       (actor-remove-condition-of-type! actor 'envenomed))
     ]
    ))

(define (get-firearm actor)
  (define items (actor-inventory actor))
  (findf (λ (item) (ranged-weapon? item))
         items)
  )

(define (make-pc-actor
         name
         #:max-hp max-hp
         #:size size
         #:max-lp (max-lp 0)
         #:modifications (modifications '())
         #:sense-organs (sense-organs '())
         #:manipulators (manipulators '())
         )
  (current-last-numeric-actor-id++)
  (define hunger 30)
  (define fatigue 70)
  (pc-actor*
   'pc name 'pc
   max-hp max-hp size
   ; attributes
   '() '() '() '() '()
   ; traits etc
   (make-hash) '() '() '() '() '()

   ; pc-actor-fields
   max-lp max-lp 6 #t '() 0
   hunger
   fatigue
   modifications
   sense-organs
   manipulators
   ))

(define (pc-take-damage! actor damage damage-type)
  (when (< damage 0) (error "pc-take-damage: damage cannot be less than 0"))

  (cond [(not (positive? (actor-hp actor)))

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
               )]

        [else
         (define new-hp (- (actor-hp actor) damage))
         (when (not (positive? new-hp))
           (notice "Otava is dying.")
           (wait-for-confirm))

         (set-actor-hp! actor new-hp)
         (notice (format "Otava takes ~a damage. New HP: ~a." damage (actor-hp actor)))
         'hit]))


; TODO: dispatching?
(define (make-enemy type)
  (case type
    ['grabberkin (make-grabberkin)]
    ['blindscraper (make-blindscraper)]
    ['human-fighter (make-human-fighter)]
    ['voidfloater (make-voidfloater)]
    ['limbtearer (make-limbtearer)]
    [else (dev-note (format "Unknown enemy type: ~a" type))]))

; TODO: make stances nice
(define (spawn type number range)
  (for ([i (in-range 0 number)])
    (define sign
      (if (= number 1)
          ""
          (case i
            [(0) "α"]
            [(1) "β"]
            [(2) "γ"]
            [(3) "δ"]
            [(4) "ε"]
            [else (number->string (add1 i))])))

    (define enemy (make-enemy type))

    (define description
          (case i
            [(0) "right"]
            [(1) "left"]
            [else "right"]))
    (define enemy-stance
      (stance sign range description))
    (set-actor-stance! enemy enemy-stance)

    (move-actor-to-location! enemy (current-location))
    (notice (format "~a A ~a appears ~a." (timestamp) (string-downcase (actor-name enemy)) range))
    )

  (current-times-species-encountered++ type)
)

(define (actor-stance-range actor)
  (stance-range (actor-stance actor)))

; it's always the enemy's stance that's modified, but
; the action might be initiated by the pc
(define (set-actor-stance-range! moving-actor new-range [pc? #f])
  (define old-stance (actor-stance moving-actor))
  (when (and (not (equal? (stance-range (actor-stance moving-actor)) new-range))
             (not (and (equal? new-range 'engaged)
                       (not (null? (get-enemies-at-range 'engaged))))))
    (set-stance-range! (actor-stance moving-actor) new-range)
    (notice
     (format "[~a ι] ~a" (current-elapsed-time) (format
      (case (stance-range (actor-stance moving-actor))
        ['engaged
         (if pc?
             "Otava is now grappling with the ~a."
             "The ~a is now grappling with Otava."
             )
         ]
        ['adjacent
         (if pc?
             "Otava is now adjacent to the ~a."
             "The ~a is now adjacent to Otava."
             )
         ]
        ['close
         (if pc?
             "Otava is now close to the ~a."
             "The ~a is now close to Otava."
             )
         ]
        ['nearby
         (if pc?
             "Otava is now near ~a."
             "The ~a is now nearby."
             )
         ]
        ['far
         (if pc?
             "Otava is now far from ~a."
             "The ~a is now far."
             )
         ])
      (actor-name moving-actor)))
     )
    )
  )
