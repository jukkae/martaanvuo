#lang racket

(provide (all-defined-out))

(provide (all-from-out "pending-action.rkt"
                       "resolve-counts.rkt"))

(require racket/lazy-require)
(require racket/serialize)

(require "../action.rkt")
(require "../actor.rkt")
(require "../condition.rkt")
(require "../io.rkt")
(require "../location.rkt")
(require "../pc.rkt")
(require "../place.rkt")
(require "../quest.rkt")
(require "../route.rkt")
(require "../stance.rkt")
(require "../status.rkt")
(require "../utils.rkt")
(require "../world.rkt")

(require "combat.rkt"
         "logging.rkt"
         "pending-action.rkt"
         "resolve-counts.rkt")


(serializable-struct
 situation
 ([world #:mutable]
  [pc #:mutable]
  [life #:mutable]
  [run #:mutable]
  [round #:mutable]
  [elapsed-time #:mutable]
  [in-combat? #:mutable]
  [current-fragment-id #:mutable]
  [quests #:mutable]
  [persistent-quests #:mutable]
  [grabberkin-encounters #:mutable]
  [flags #:mutable]

  ; these are place-place pairs, from-to
  [times-begin-traverse-narrated #:mutable]
  [times-finish-traverse-narrated #:mutable]
  [times-cancel-traverse-narrated #:mutable]
  )
 #:transparent)

; logging
(define current-log (make-parameter '()))
(define current-part (make-parameter 0))
(define current-chapter (make-parameter 0))
(define current-last-paragraph (make-parameter ""))
(define current-prompt (make-parameter ""))

(define current-pending-action (make-parameter '()))

(define current-times-begin-traverse-narrated (make-parameter (make-hash)))
(define current-times-finish-traverse-narrated (make-parameter (make-hash)))
(define current-times-cancel-traverse-narrated (make-parameter (make-hash)))


;;; Actual state variables
(define *situation* '())

(define (reset-situation!)
  (set! *situation*
        (let ([new-world (world 0 0)]
              [pc (make-new-pc)]
              [quests '()]
              [persistent-quests '()])
          (situation new-world
                     pc
                     0
                     0
                     0
                     0
                     #f
                     '()
                     quests
                     persistent-quests
                     0
                     '()
                     (make-hash)
                     (make-hash)
                     (make-hash)))))


;;; Meta progression / achievements
(define (increment-achievement! achievement)
  (case achievement
    ['forgetful (displayln "forgetful achievement incremented")]
    [else
     (displayln "increment-achievement!: unknown achievement:")
     (displayln achievement)]))

(define (unset-current-fragment-id!)
  (set-situation-current-fragment-id! *situation* '()))

(define (set-current-fragment-id! id)
  (set-situation-current-fragment-id! *situation* id))

(define (add-quest! quest)
  (set-situation-quests!
   *situation*
   (append-element (situation-quests *situation*) quest)))

(define (quest-exists? id)
  (define quests (situation-quests *situation*))
  (if (null? quests)
      #f
      (findf (λ (quest) (eq? id (quest-id quest))) quests)))

; set-quest-status! is obviously reserved
(define (update-quest-status! id status)
  (define quests (situation-quests *situation*))
  (define quest (findf (λ (quest) (eq? id (quest-id quest))) quests))
  (when quest
    (set-quest-status! quest status)
    )
  )

; would be nice to add instead of overwrite, but that requires smart linebreaking in info-cards
(define (update-quest-notes! id notes)
  (define quests (situation-quests *situation*))
  (define quest (findf (λ (quest) (eq? id (quest-id quest))) quests))
  (when quest
    (set-quest-notes! quest notes)
    )
  )

(define (update-quest-details! id details)
  (define quests (situation-quests *situation*))
  (define quest (findf (λ (quest) (eq? id (quest-id quest))) quests))
  (when quest
    (set-quest-details! quest details)
    )
  )

; api
(define (current-location)
  (actor-location (pc)))


; api
(define (get-current-enemies)
  (filter
   (λ (actor) (and (actor-alive? actor)
                   (not (pc-actor? actor))))
   (location-actors (current-location))))

; api
(define (quests)
  (situation-quests *situation*))

(define (find-quest id)
  (findf (λ (q) (eq? (quest-id q) id))
         (situation-quests *situation*)))



(define (reduce-debt-by! amount)
  (define debt-quest (find-quest 'pay-off-debt))
  
  (define old-debt-amount (quest-details debt-quest))
  (define new-debt-amount (- old-debt-amount amount))
  (set-quest-details! debt-quest new-debt-amount)
  (set-quest-notes! debt-quest
                    (string-append
                     "unsettled: "
                     (number->string new-debt-amount)
                     " g of Martaanvuo gold"))
  (displayln "new-debt-amount:")
  #;(displayln (~r new-debt-amount)) ; formatting todo
  (displayln new-debt-amount)
  
  '())

; API
(define (engaged?)
  (define any-enemy-engaged? #f)
  
  (for ([enemy (get-current-enemies)])
    (let ([stance (actor-stance enemy)])
      (when (eq? (stance-range stance) 'engaged)
        (set! any-enemy-engaged? #t)))
    )
  any-enemy-engaged?)

; API
(define (get-an-enemy-at-range range)
  (define current-enemies (get-current-enemies))
  (define enemies-shuffled (shuffle current-enemies))
  (define enemy-in-range '())
  (for ([enemy enemies-shuffled])
    (define stance (actor-stance enemy))
    (when (eq? (stance-range stance) range)
      (set! enemy-in-range enemy)))
  enemy-in-range)

; API
(define (get-enemies-at-range range)
  (define current-enemies (get-current-enemies))
  (define enemies-in-range '())
  (for ([enemy current-enemies])
    (define stance (actor-stance enemy))
    (when (eq? (stance-range stance) range)
      (set! enemies-in-range (append-element enemies-in-range enemy))))
  enemies-in-range)

; API
(define (in-range? target attack-mode)
  (case attack-mode
    ['melee #t]
    [else (displayln "in-range? not implemented yet for this attack mode")]))


(define (describe-non-combat-situation)
  (cond ((null? (situation-current-fragment-id *situation*))
         (cond ((eq? (location-id (current-location)) 'perimeter)
                (set-prompt! "Either a climb up the rocky slope to the magpie, or follow the ants to the swamp."))
               ((eq? (location-id (current-location)) 'magpie-hill)
                (p "Natural rock stairs lead back to Perimeter. There's a decrepit industrial building further ahead on the plateau in the fog. There's also a small trail that seems to lead down, towards Martaanvuo swamp.")))
         (cond ((location-has-feature? (current-location) 'magpie-effigy)
                (p "\"Chk-chk\", the magpie calls insistently from the foliage of the skeletonlike forest on the plateau."))))))

(define (clean-situation!)
  (dev-note "cleaning situation...")
  (reset-pending-action!)
  (set-situation-quests! *situation* '()))


(define (describe-situation)
  (when (location-has-feature? (current-location) 'locked-door)
    (cond ((and (pc-has-item? 'revolver)
                (pc-has-ammo-left?))
           (p "There's a door that's locked with a heavy padlock."))
          ((and (pc-has-item? 'bolt-cutters))
           (p "There's a door that's locked with a heavy padlock."))
          (else
           (p "There's a door that's locked with a heavy padlock. If only she had bolt cutters, or something."))))
  (cond
    ((in-combat?) (describe-combat-situation))
    (else (describe-non-combat-situation)))
  )

(define (redescribe-situation)
  (cond
    ((in-combat?) (describe-combat-situation))
    (else (repeat-last-paragraph)))
  )

; scripting API / situation
(provide pc)
(define (pc)
  (situation-pc *situation*))

; scripting API / situation
(provide in-combat?)
(define (in-combat?)
  (situation-in-combat? *situation*))


; scripting API / situation / implementation detail
(define (remove-all-enemies-and-end-combat!)
  (for ([enemy (get-current-enemies)])
    (remove-actor-from-location! (actor-location enemy) enemy))
  (end-combat!)
  (displayln "post-combat steps") ; for instance, wound care (fast vs good), xp, summary etc
  )

; scripting API
(define (remove-enemy enemy)
  (remove-actor-from-location! (actor-location enemy) enemy))

; scripting API
(provide actor-in-range?)
(define (actor-in-range? enemy range)
  (define stance (actor-stance enemy))
  (eq? (stance-range stance) range))


; scripting API
(provide award-xp!)
(define (award-xp! amount . reason)
  (if (null? reason)
      (displayln (string-append "[+" (number->string amount) " xp]"))
      (displayln (string-append "[+" (number->string amount) " xp: " (car reason) "]")))
  (define pc (situation-pc *situation*))
  (set-pc-actor-xp! pc
                    (+ (pc-actor-xp pc)
                       amount)))

; scripting API?
(define (player-info)
  (define player-status
    (list
     (list " life " (string-append " " (number->string (situation-life *situation*)) " "))
     (list " grabberkin encounters " (string-append " " (number->string (situation-grabberkin-encounters *situation*)) " "))
     ))
     
  (info-card player-status (string-append "Player status"))
  )


; scripting API
(define (end-game)
  (wait-for-confirm)
  (p "[The end.]")
  (player-info)
  (wait-for-confirm)
  (exit))

; api?
(define (pick-up-items!)
  (p "Otava picks up everything there is to pick up.")
  (define all-items (place-items (current-location)))
  (for ([item all-items])
    (remove-item-from-location! (current-location) item)
    (add-item-to-inventory! (pc) item))
  (print-inventory))


(define (set-flag flag)
  (when (not (flag-set? flag))
    (set-situation-flags! *situation* (append-element (situation-flags *situation*) flag))))

(define (remove-flag flag)
  (when (flag-set? flag)
    (set-situation-flags! *situation* (remq flag (situation-flags *situation*)))))

(define (flag-set? flag)
  (memq flag (situation-flags *situation*)))

(define (print-flags)
  (displayln "print-flags:")
  (displayln (situation-flags *situation*)))




(define (save)
  (save-situation *situation*))

(serializable-struct state
                     ([situation #:mutable]
                      [log #:mutable]
                      [last-paragraph #:mutable]
                      [part #:mutable]
                      [chapter #:mutable]
                      [prompt #:mutable]
                      [pending-action #:mutable]
                      [times-begin-traverse-narrated #:mutable]
                      [times-finish-traverse-narrated #:mutable]
                      [times-cancel-traverse-narrated #:mutable]
                      ))

(define (save-situation s)
  
  (define serialized-situation (serialize s))
  #;(write-save-file serialized-situation)

  (define output-file (open-output-file "save.txt" #:exists 'truncate)) ; truncate = delete if exists

  (define st (state
              *situation*
              (current-log)
              (current-last-paragraph)
              (current-part)
              (current-chapter)
              (current-prompt)
              (current-pending-action)
              (current-times-begin-traverse-narrated)
              (current-times-finish-traverse-narrated)
              (current-times-cancel-traverse-narrated)))
  (define serialized-state (serialize st))
  (write serialized-state output-file)

  
  #;(write serialized-situation output-file)
  (close-output-port output-file))

; NOTE: "Serialization followed by deserialization produces a value with the same graph structure and mutability as the original value, but the serialized value is a plain tree (i.e., no sharing)."
; - https://docs.racket-lang.org/reference/serialization.html
(define (load-situation situation)
  #;(displayln situation)
  (define deserialized (deserialize situation))
  (set! *situation* deserialized))


(define (load-situation-from-state serialized-state)
  #;(displayln situation)
  (define deserialized-state (deserialize serialized-state))
  (define situation (state-situation deserialized-state))
  
  (current-log (state-log deserialized-state))
  (current-last-paragraph (state-last-paragraph deserialized-state))
  (current-part (state-part deserialized-state))
  (current-chapter (state-chapter deserialized-state))
  (current-prompt (state-prompt deserialized-state))
  (current-pending-action (state-pending-action deserialized-state))
  (current-times-begin-traverse-narrated (state-times-begin-traverse-narrated deserialized-state))
  (current-times-finish-traverse-narrated (state-times-finish-traverse-narrated deserialized-state))
  (current-times-cancel-traverse-narrated (state-times-cancel-traverse-narrated deserialized-state))
  
  (set! *situation* situation))