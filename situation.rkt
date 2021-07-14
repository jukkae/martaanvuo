#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "action.rkt")
(require "actor.rkt")
(require "condition.rkt")
(require "io.rkt")
(require "location.rkt")
(require "pc.rkt")
(require "quest.rkt")
(require "stance.rkt")
(require "status.rkt")
(require "utils.rkt")
(require "world.rkt")


;;; Types
(serializable-struct
 situation
 ([world #:mutable]
  [pc #:mutable]
  [life #:mutable]
  [run #:mutable]
  [round #:mutable]
  [elapsed-time #:mutable]
  [in-combat? #:mutable]
  [enemy-stances #:mutable]
  [current-fragment #:mutable]
  [quests #:mutable]
  [persistent-quests #:mutable]
  [grabberkin-encounters #:mutable]
  [pending-action #:mutable]
  ))


;;; Actual state variables
(define *situation*
  (let ([new-world (world 0 0)]
        [pc (make-new-pc)]
        [quests '()]
        [persistent-quests '()])
    (situation new-world pc 0 0 0 0 #f '() '() quests persistent-quests 0 '())))
;;; ^^^


(define (add-stance! stance)
  (set-situation-enemy-stances!
   *situation*
   (append-element (situation-enemy-stances *situation*) stance)))

(define (remove-stance! enemy)
  (set-situation-enemy-stances!
   *situation*
   (filter
    (λ (stance) (not (equal? (stance-enemy stance)
                             enemy))) ; equal? compares opaque structs by identity
    (situation-enemy-stances *situation*))))

(define (find-stance enemy)
  (findf
   (λ (stance) (equal? (stance-enemy stance)
                       enemy)) ; equal? compares opaque structs by identity
   (situation-enemy-stances *situation*)))



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
  (set-situation-pending-action! *situation* action))

(define (add-quest! quest)
  (set-situation-quests!
   *situation*
   (append-element (situation-quests *situation*) quest)))

(define (quest-exists? id)
  (define quests (situation-quests *situation*))
  (findf (λ (quest) (eq? id (quest-id quest))) quests))

;;; Constructors
(define (create-quest quest-symbol)
  (define q
    (case quest-symbol
      ['pay-off-debt
       (quest 'pay-off-debt
              "pay off the debt to the Collector"
              "in progress"
              "unsettled: 10,111 grams of U-235")] ; 10,111 is the 1,242th prime number, binary 10111 = 23
      ['the-anthead
       (quest 'the-anthead
              "seek the Anthead Girl"
              "not started"
              "\"not ready yet\", whatever.")]))
  (add-quest! q)


  (case quest-symbol
      ['pay-off-debt
       (paragraph "She's got a feeling that the Martaanvuo facility might be the big one, the one that sets her free. Or the one that kills her. Otava sighs and gathers pace.")]
      )
  

  (define body
    (format-quest-for-card q))

  (info-card
   (list body)
   "New quest")

  (wait-for-confirm))

;;; plumbing for round-resolver
(define (get-continue-pending-action-name)
  (define pending-action (situation-pending-action *situation*))
  (cond ((eq? (action-symbol pending-action) 'go-to-location)
         (string-append
          "[continue] Continue towards "
          (get-location-name-from-location (action-target pending-action))
          "."))
        ((eq? (action-symbol pending-action) 'search-for-paths)
         (string-append
          "[continue] Search for paths."))
        (else (string-append "[continue] unknown action symbol: " (symbol->string (action-symbol pending-action))))))


; api
(define (current-location)
  (actor-current-location (pc)))


; api
(define (get-current-enemies)
  (filter
   (λ (actor) (and (actor-alive? actor)
                   (not (pc-actor? actor))))
   (location-actors (current-location))))

; api
(define (quests)
  (situation-quests *situation*))


; combat?
(define (get-combatant-name actor)
  (cond ((pc-actor? actor)
         "Otava")
        (else
         (define stance (find-stance actor))
         (cond ((= (length (situation-enemy-stances *situation*)) 1)
                (append-string (actor-name actor)))
               (else
                (define name (actor-name actor))
                (define index
                  (if stance
                      (stance-index stance)
                      ""))
                (append-string name " " index))))))

(define (display-non-pc-combatant-info actor)
  (define stance (find-stance actor))
  (define name (get-combatant-name actor))
  (define hide-hp?
    (if (hash-ref (actor-traits actor) "hp-hidden" #f)
        #t
        #f))

  (define body
    (case (actor-name actor)
      [("Grabberkin")
       (list
        (list
         " HP "
         (if hide-hp?
             " ??? "
             (string-append " "
                            (number->string (actor-hp actor))
                            "/"
                            (number->string (actor-max-hp actor))
                            " "
                            ))))]
      [("Blindscraper")
       (list
        (list
         " size "
         (string-append " "
                        (get-trait actor "size")
                        " "
                        ))
        #;(list
           " location "
           (string-append " " (stance-location stance) " "))
        (if (not (null? stance))
            (list
             " range "
             (string-append " " (symbol->string (stance-range stance)) " "))
            (list
             " range "
             (string-append " " "N/A" " ")))
        

        )]))

  (when (not (null? (actor-statuses actor)))
    (define statuses (actor-statuses actor))
    (define statuses-list
      (list " statuses " (~s statuses)))
    (set! body (append-element body statuses-list)))
  (info-card
   body
   name))

; API
(define (engaged?)
  (define any-enemy-engaged? #f)
  (for ([stance (situation-enemy-stances *situation*)])
    (when (eq? (stance-range stance) 'engaged)
      (set! any-enemy-engaged? #t)))
  any-enemy-engaged?)

; API
(define (get-an-enemy-at-range range)
  (define current-enemies (get-current-enemies))
  (define enemies-shuffled (shuffle current-enemies))
  (define enemy-in-range '())
  (for ([enemy enemies-shuffled])
    (define stance (find-stance enemy))
    (when (eq? (stance-range stance) range)
      (set! enemy-in-range enemy)))
  enemy-in-range)

; API
(define (get-enemies-at-range range)
  (define current-enemies (get-current-enemies))
  (define enemies-in-range '())
  (for ([enemy current-enemies])
    (define stance (find-stance enemy))
    (when (eq? (stance-range stance) range)
      (set! enemies-in-range (append-element enemies-in-range enemy))))
  enemies-in-range)

; API
(define (in-range? target attack-mode)
  (case attack-mode
    ['melee #t]
    [else (displayln "in-range? not implemented yet for this attack mode")]))

; Combat?
(define (display-pc-combatant-info actor)
  (define name (get-combatant-name actor))
  (define body
    (list
     (list
      " HP "
      (string-append " "
                     (number->string (actor-hp actor))
                     "/"
                     (number->string (actor-max-hp actor))
                     " "
                     ))))

  (when (not (null? (actor-statuses actor)))
    (define statuses (actor-statuses actor))
    (define statuses-strings
      (for/list ([status statuses])
        (string-append "["
                       (symbol->string (status-type status))
                       " ("
                       (number->string (status-lifetime status))
                       ")]")))
    
    (define statuses-list
      (list " statuses "
            (string-append " " (string-join statuses-strings) " ")))
    (set! body (append-element body statuses-list)))

  (when (not (null? (actor-conditions actor)))
    (define conditions (actor-conditions actor))
    (define conditions-strings
      (for/list ([condition conditions])
        (string-append "["
                       (symbol->string (condition-type condition))
                       "]")))
    
    (define conditions-list
      (list " conditions "
            (string-append " " (string-join conditions-strings) " ")))
    (set! body (append-element body conditions-list)))
  (info-card
   body
   name))

; Combat?
(define (display-combatant-info actor)
  (if (pc-actor? actor)
      (display-pc-combatant-info actor)
      (when (actor-alive? actor)
        (display-non-pc-combatant-info actor))))

(define (describe-combat-situation)
  (notice "Otava is in combat.")
  (for ([enemy (get-current-enemies)])
    (display-combatant-info enemy)
    
    )
  (display-pc-combatant-info (situation-pc *situation*))
  )

(define (describe-non-combat-situation)
  (when (not (situation-current-fragment *situation*))
    (cond ((eq? (location-id (current-location)) 'perimeter )
         (paragraph "Rusty machines and remains of makeshift habs litter the hostile woods of Perimeter, the last area outside Anomaly. The snaking path splits in two. The left hand path is a steep and narrow climb up Blackfang Peak. The other one descends to Martaanvuo swamp.")))))

(define (serialize-state)
  ; prng can be stored as vector:
  ; https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._pseudo-random-generator-~3evector%29%29
  '())

(define (serialize-input)
  '())


(define (clean-situation!)
  (displayln "<< clean-situation! >>")
  (reset-pending-action!)
  (set-situation-quests! *situation* '()))


(define (describe-situation)
  (when (location-has-feature? (current-location) 'locked-door)
    (cond ((and (pc-has-item? 'revolver)
                (pc-has-ammo-left?))
           (paragraph "There's a door that's locked with a heavy padlock."))
          ((and (pc-has-item? 'bolt-cutters))
           (paragraph "There's a door that's locked with a heavy padlock."))
          (else
           (paragraph "There's a door that's locked with a heavy padlock. If only she had bolt cutters, or something."))))
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
; TODO this should also purge action queue -> round-resolver needs to be informed when this gets called
(define (remove-all-enemies-and-end-combat!)
  (for ([enemy (get-current-enemies)])
    (remove-stance! enemy)
    (remove-actor-from-location! (actor-current-location enemy) enemy))
  (end-combat!)
  (displayln "post-combat steps") ; for instance, wound care (fast vs good), xp, summary etc
  )

; scripting API
(define (remove-enemy enemy)
  (remove-stance! enemy)
  (remove-actor-from-location! (actor-current-location enemy) enemy))

; scripting API
(provide actor-in-range?)
(define (actor-in-range? enemy range)
  (define stance (find-stance enemy))
  (eq? (stance-range stance) range))

; infrastructure / location?
(provide move-pc-to-location!)
(define (move-pc-to-location! location)
  ; TODO: location on-exit / on-enter triggers here
  #;(displayln (string-append "-- move-pc-to-location!: moving to " (~v location)))
  (remove-actor-from-its-current-location! (situation-pc *situation*))
  (set-actor-current-location! (situation-pc *situation*) location)
  (add-actor-to-location! location (situation-pc *situation*)))


; infrastructure, not scripting api
(provide clean-up-dead-actor!)
(define (clean-up-dead-actor! actor)
  (remove-stance! actor)
  (set-location-actors! (current-location) (remove actor (location-actors (current-location))))
  (define corpse (cons 'corpse "Corpse (TODO)"))
  (displayln "clean-up-dead-actor!: todo: add corpse")
  #;(displayln corpse))

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

; Scripting API
(define (inflict-status! target status)
  (match (status-type status)
    ['blind
     (displayln "todo: blind should be a condition, not a status")
     (paragraph "The Blindscraper swings its claw through an opening between Otava's arms. The claw tears diagonally across Otava's face, cutting its way through flesh, scraping bone.")
     (define roll (d 1 2))
     (wait-for-confirm)
     (case roll
       [(1)
        ; -> next generation: scars where there were wounds, then next: tattoos -> with both giving changes to the build - "the ghost that lived through" (it's often possible to name a reason)
        (paragraph "A searing pain cuts through her left eye. Blood and intraocular fluid gush down her face.")]
       [(2)
        (paragraph "A searing pain cuts through her eyes as her vision turns to black.")])
     ]
    ['bound
     (actor-set-status! target (status-type status) (status-lifetime status))
     ]
    [else (paragraph "todo: unknown status")]))

(define (inflict-condition! target cond)
  (match (condition-type cond)
    ['ankle-broken
     (if (actor-has-condition-of-type? target 'ankle-broken)
         (begin
           (actor-remove-condition-of-type! target 'ankle-broken)
           (actor-add-condition! target (condition 'both-ankles-broken "TODO" (λ () '())))
           )
         (actor-add-condition! target cond))
     ]
    ['bleeding
     (if (not (actor-has-condition-of-type? target 'bleeding))
         (actor-add-condition! target cond)
         (displayln "Already bleeding."))
     
     ]
    [else (paragraph "todo: unknown condition")]))

; scripting API
(define (end-game)
  (wait-for-confirm)
  (paragraph "[The end.]")
  (player-info)
  (wait-for-confirm)
  (exit))

; api?
(define (pick-up-items!)
  (paragraph "Otava picks up everything there is to pick up.")
  (define all-items (location-items (current-location)))
  (for ([item all-items])
    (remove-item-from-location! (current-location) item)
    (add-item-to-inventory! (pc) item))
  (print-inventory))