#lang racket

(provide (all-defined-out))

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

(require "combat.rkt")


; Think about breaking this apart:
; - run-specific
; - life-specific
; - playthrough-specific
; - in-world / out-of-world (ie., narration)
; - player knowledge vs character knowledge
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
  [pending-action #:mutable]
  [log #:mutable]
  [last-paragraph #:mutable]
  [current-part #:mutable]
  [current-chapter #:mutable]
  [prompt #:mutable]
  [flags #:mutable]

  ; these are place-place pairs, from-to
  [times-begin-traverse-narrated #:mutable]
  [times-finish-traverse-narrated #:mutable]
  [times-cancel-traverse-narrated #:mutable]
  )
 #:transparent)

;; name: hmm
(define (times-begin-traverse-narrated++ key) 
  (when (not (hash-has-key? (situation-times-begin-traverse-narrated *situation*) key))
    (hash-set! (situation-times-begin-traverse-narrated *situation*) key 0))
  (hash-set! (situation-times-begin-traverse-narrated *situation*)
             key
             (add1 (hash-ref (situation-times-begin-traverse-narrated *situation*)
                             key))))

(define (times-begin-traverse-narrated key) 
  (if (not (hash-has-key? (situation-times-begin-traverse-narrated *situation*) key))
      #f
      (hash-ref (situation-times-begin-traverse-narrated *situation*)
                key)))

(define (times-finish-traverse-narrated++ key) 
  (when (not (hash-has-key? (situation-times-finish-traverse-narrated *situation*) key))
    (hash-set! (situation-times-finish-traverse-narrated *situation*) key 0))
  (hash-set! (situation-times-finish-traverse-narrated *situation*)
             key
             (add1 (hash-ref (situation-times-finish-traverse-narrated *situation*)
                             key))))

(define (times-finish-traverse-narrated key) 
  (when (not (hash-has-key? (situation-times-finish-traverse-narrated *situation*) key))
    #f)
  (hash-ref (situation-times-finish-traverse-narrated *situation*)
            key))

(define (times-cancel-traverse-narrated++ key) 
  (when (not (hash-has-key? (situation-times-cancel-traverse-narrated *situation*) key))
    (hash-set! (situation-times-cancel-traverse-narrated *situation*) key 0))
  (hash-set! (situation-times-cancel-traverse-narrated *situation*)
             key
             (add1 (hash-ref (situation-times-cancel-traverse-narrated *situation*)
                             key))))

(define (times-cancel-traverse-narrated key) 
  (when (not (hash-has-key? (situation-times-cancel-traverse-narrated *situation*) key))
    #f)
  (hash-ref (situation-times-cancel-traverse-narrated *situation*)
            key))


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
                     '()
                     '()
                     0
                     0
                     ""
                     '()
                     (make-hash)
                     (make-hash)
                     (make-hash)))))


; NOTE: "Serialization followed by deserialization produces a value with the same graph structure and mutability as the original value, but the serialized value is a plain tree (i.e., no sharing)."
; - https://docs.racket-lang.org/reference/serialization.html
(define (load-situation situation)
  #;(displayln situation)
  (define deserialized (deserialize situation))
  (set! *situation* deserialized))




;;; Meta progression / achievements
(define (increment-achievement! achievement)
  (case achievement
    ['forgetful (displayln "forgetful achievement incremented")]
    [else
     (displayln "increment-achievement!: unknown achievement:")
     (displayln achievement)]))

;;; Combat
;;; (or actually, eventually, any kind of action scene, but more about that later)
(define *combat-flags* '())
(define (add-combat-flag flag)
  (set! *combat-flags* (append-element *combat-flags* flag)))

(define (begin-combat!)
  #;(displayln "BEGIN COMBAT")
  (set-situation-in-combat?! *situation* #t)
  (set! *combat-flags* '()))

(define (end-combat!)
  #;(displayln "END COMBAT")
  (notice "Combat finished.")
  (set-situation-in-combat?! *situation* #f)
  (set! *combat-flags* '()))


;;; Direct accessors and mutators
(define (reset-pending-action!)
  (set-situation-pending-action! *situation* '()))

(define (set-pending-action! action)
  #;(symbol
     actor
     duration
     target
     tags
     details)
  (set-situation-pending-action! *situation* action))

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


;;; plumbing for round-resolver
(define (get-continue-pending-action-name)
  (define pending-action (situation-pending-action *situation*))
  (cond ((eq? (action-symbol pending-action) 'go-to-location)
         (string-append
          "[continue] Continue towards "
          (get-location-name-from-location (action-target pending-action))
          "."))
        ((eq? (action-symbol pending-action) 'traverse)
         (define target (action-target pending-action))
         
         (define details (action-details pending-action))
         
         (define direction
           (if (memq 'a-to-b details)
               'a-to-b
               'b-to-a))

         (define endpoint
           (case direction
             ['a-to-b (route-b target)]
             ['b-to-a (route-a target)]))
         
         (string-append
          "[continue] Continue towards "
          (get-location-name-from-location endpoint)
          "."))
        ((eq? (action-symbol pending-action) 'search-for-paths)
         (string-append
          "[continue] Search for paths."))
        (else (string-append "[continue] unknown action symbol: " (symbol->string (action-symbol pending-action))))))

(define (get-cancel-pending-action-and-go-back-name
         route
         pending-action)
  ; this assumes that pending-action is 'traverse, which might not be the case
  ;(define end-location (action-target pending-action))
  ; not very robust... anyhow, cancel direction is opposite to the pending action direction
  (define cancel-traverse-direction
    (if (memq 'b-to-a (action-details pending-action))
        'a-to-b
        'b-to-a))

  (define cancel-traverse-endpoint
    (case cancel-traverse-direction
      ['a-to-b (route-b route)]
      ['b-to-a (route-a route)]))
  
  (string-append "Go back to " (get-location-name-from-location cancel-traverse-endpoint) "."))

(define (get-cancel-and-go-back-destination
         route
         pending-action)
  (define cancel-traverse-direction
    (if (memq 'b-to-a (action-details pending-action))
        'a-to-b
        'b-to-a))

  (define cancel-traverse-endpoint
    (case cancel-traverse-direction
      ['a-to-b (route-b route)]
      ['b-to-a (route-a route)]))
  cancel-traverse-endpoint)


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
  (displayln "<< clean-situation! >>")
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


(define (next-chapter!)
  (when (= (situation-current-part *situation*) 0)
    (set-situation-current-part! *situation* 1))
  (set-situation-current-chapter! *situation* (add1 (situation-current-chapter *situation*)))
  (print-heading))

(define (next-part!)
  (set-situation-current-part! *situation* (add1 (situation-current-part *situation*)))
  (set-situation-current-chapter! *situation* 0)
  (print-heading))

(define (append-to-log paragraph)
  (set-situation-log! *situation* (append-element (situation-log *situation*) paragraph)))

(define (get-log)
  (situation-log *situation*))

(define (get-last-paragraph)
  (situation-last-paragraph *situation*))

(define (set-last-paragraph! paragraph)
  (set-situation-last-paragraph! *situation* paragraph)
  )

(define (set-prompt! prompt)
  (set-situation-prompt! *situation* prompt))

(define (get-prompt)
  (situation-prompt *situation*))


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

(define (save-situation s)
  ; prng can be stored as vector:
  ; https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._pseudo-random-generator-~3evector%29%29
  ; NOTE: By storing the prng in savefile, you essentially get predestination
  ; -> this opens the possibility for deliberately using this as a mechanic
  (define serialized-situation (serialize s))
  (write-save-file serialized-situation))