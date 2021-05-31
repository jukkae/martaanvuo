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
  (let ([new-world (world (list edgeflats swamp ridges valleys crematory ruins sewers cache workshop spring) 0 0)]
        [pc (make-new-pc)]
        [quests '()]
        [persistent-quests '()])
    (situation new-world pc 0 0 0 0 #f (make-hash) '() quests persistent-quests 0 '())))
;;; ^^^



;;; Direct accessors and mutators
(define (reset-pending-action!)
  (set-situation-pending-action! *situation* '()))
(define (set-pending-action! action)
  (set-situation-pending-action! *situation* action))

(define (add-quest! quest)
  (set-situation-quests!
   *situation*
   (append-element (situation-quests *situation*) quest)))

;;; Constructors
(define (create-quest quest-symbol)
  (define q
    (case quest-symbol
      ['pay-off-debt
       (quest 'pay-off-debt
              "pay off the debt to the Collector"
              "in progress"
              "unsettled: 4,328 grams of U-235")]
      ['the-anthead
       (quest 'the-anthead
              "seek the Anthead Girl"
              "not started"
              "\"not ready yet\"")]))
  (add-quest! q)

  (define body
    (format-quest-for-card q))

  (info-card
   (list body)
   "New quest")
  )

;;; plumbing for round-resolver
(define (get-continue-pending-action-name)
  (define pending-action (situation-pending-action *situation*))
  (cond ((eq? (action-symbol pending-action) 'go-to-location)
         (string-append
          "Continue towards "
          (get-location-name-from-location-type (location-type (action-target pending-action)))
          "."))
        ((eq? (action-symbol pending-action) 'search-for-paths)
         (string-append
          "Keep on searching for paths."))
        (else (string-append "get-continue-pending-action-name: unknown action symbol: " (symbol->string (action-symbol pending-action))))))


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
         (define stance (hash-ref! (situation-enemy-stances *situation*) actor '()))
         (cond ((= (hash-count (situation-enemy-stances *situation*)) 1)
                (append-string (actor-name actor)))
               (else
                (define name (actor-name actor))
                (define index (stance-index stance))
                (append-string name " " index))))))

(define (display-non-pc-combatant-info actor)
  (define stance (hash-ref! (situation-enemy-stances *situation*) actor '()))
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
  (for ([(k stance) (in-hash (situation-enemy-stances *situation*))])
    (when (eq? (stance-range stance) 'engaged)
      (set! any-enemy-engaged? #t)))
  any-enemy-engaged?)

; API
(define (get-an-enemy-at-range range)
  (define current-enemies (get-current-enemies))
  (define enemies-shuffled (shuffle current-enemies))
  (define enemy-in-range '())
  (for ([enemy enemies-shuffled])
    (define stance (hash-ref (situation-enemy-stances *situation*) enemy '()))
    (when (eq? (stance-range stance) range)
      (set! enemy-in-range enemy)))
  enemy-in-range)

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
                       " ("
                       "condition details TBD"
                       ")]")))
    
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
      (display-non-pc-combatant-info actor)))

(define (describe-combat-situation)
  (paragraph "Otava is in combat.")
  (for ([enemy (get-current-enemies)])
    (display-combatant-info enemy)
    
    )
  (display-pc-combatant-info (situation-pc *situation*))
  )

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
  (cond
    ((in-combat?) (describe-combat-situation)))
  )

(define (redescribe-situation)
  (cond
    ((in-combat?) (describe-combat-situation))
    (else (displayln "redescribe-situation: TODO")))
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
(provide set-in-combat?!)
(define (set-in-combat?! in-combat?)
  (set-situation-in-combat?! *situation* in-combat?))

; scripting API / situation / implementation detail
(define (remove-all-enemies-and-end-combat!)
  (for ([enemy (get-current-enemies)])
    (hash-remove! (situation-enemy-stances *situation*) enemy)
    (remove-actor-from-location! (actor-current-location enemy) enemy))
  (set-situation-in-combat?! *situation* #f))

; scripting API
(provide actor-in-range?)
(define (actor-in-range? enemy range)
  (define stance (hash-ref (situation-enemy-stances *situation*) enemy))
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
  (hash-remove! (situation-enemy-stances *situation*) actor)
  (set-location-actors! (current-location) (remove actor (location-actors (current-location))))
  (define corpse (cons 'corpse "Corpse (TODO)"))
  (displayln "clean-up-dead-actor!: todo: add corpse")
  #;(displayln corpse))

; scripting API
(provide award-xp!)
(define (award-xp! amount . reason)
  (if (null? reason)
      (displayln (string-append "[+" (number->string amount) " xp]"))
      (displayln (string-append "[+" (number->string amount) " xp " (car reason) "]")))
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
     ;(paragraph "The Grabberkin tightens its grip around Otava's ankle.")
     (actor-add-status! target status)
     ]
    [else (paragraph "todo: unknown status")]))

(define (inflict-condition! target cond)
  (match (condition-type cond)
    ['ankle-broken
     (if (actor-has-condition-of-type? target 'ankle-broken)
         (actor-remove-condition! target 'ankle-broken)
         (actor-add-condition! target (condition 'both-ankles-broken "TODO")))
     
     ]
    ['bleeding
     (if (not (actor-has-condition-of-type? target 'bleeding))
         (actor-add-condition! target (condition 'bleeding "TODO"))
         (displayln "Already bleeding."))
     
     ]
    [else (paragraph "todo: unknown condition")]))