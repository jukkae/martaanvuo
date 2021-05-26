#lang racket

(require racket/generator)
(require racket/serialize)

(require lens)
(require text-table)

(require "action.rkt")
(require "actor.rkt")
(require "blindscraper.rkt")
(require "character-sheet.rkt")
(require "fragment.rkt")
(require "fragments.rkt")
(require "grabberkin.rkt")
(require "info-card.rkt")
(require "location.rkt")
(require "pc.rkt")
(require "situation.rkt")
(require "utils.rkt")
(require "world.rkt")


(define (award-xp! amount . reason)
  (if (null? reason)
      (displayln (string-append "[+" (number->string amount) " xp]"))
      (displayln (string-append "[+" (number->string amount) " xp " (car reason) "]")))
  (define pc (situation-pc *situation*))
  (set-pc-actor-xp! pc
                    (+ (pc-actor-xp pc)
                       amount)))


(define (current-fragment-on-begin-round!)
  (paragraph (story-fragment-description (situation-current-fragment *situation*)))
  )


(define (current-fragment-get-decisions)
  (filter (lambda (potential-decision)
            ((decision-requirement potential-decision)))
          (story-fragment-decisions (situation-current-fragment *situation*))))

(define (current-fragment-handle-decision! decision)

  (paragraph (decision-description decision))
  (define next-fragment (decision-next-fragment decision))

  ; brilliant idea x dirty hack
  (when (procedure? next-fragment)
    (set! next-fragment (next-fragment)))
  (cond ((number? next-fragment)
         (go-to-story-fragment next-fragment)
         )
        ((eq? 'exit next-fragment)
         (set-situation-current-fragment! *situation* '()))
        ((eq? 'exit-and-set-build-desperate next-fragment)
         (set-build! 'desperate)
         (set-situation-current-fragment! *situation* '()))
        ((eq? 'exit-and-set-build-bruiser next-fragment)
         (set-build! 'bruiser)
         (set-situation-current-fragment! *situation* '()))
        (else (error (string-append "(current-fragment-handle-decision!): next-fragment type not implemented: " (symbol->string next-fragment)))))
  )
(define (current-scene-on-end-round!)
  '()
  )

(define (go-to-story-fragment id)
  (set-situation-current-fragment! *situation* (get-fragment id))
  ((story-fragment-on-enter! (situation-current-fragment *situation*))))


(provide pc)
(define (pc)
  (situation-pc *situation*))

(provide in-combat?)
(define (in-combat?)
  (situation-in-combat? *situation*))

(provide set-in-combat?!)
(define (set-in-combat?!)
  (set-situation-in-combat?! *situation*))

(define (remove-all-enemies-and-end-combat!)
  (for ([enemy (get-current-enemies)])
    (hash-remove! *enemy-stances* enemy)
    (remove-actor-from-location! (actor-current-location enemy) enemy))
  (set-situation-in-combat?! *situation* #f))

(define (engaged?)
  (define any-enemy-engaged? #f)
  (for ([(k stance) (in-hash *enemy-stances*)])
    (when (eq? (stance-range stance) 'engaged)
      (set! any-enemy-engaged? #t)))
  any-enemy-engaged?)


(serializable-struct event
                     (type
                      details
                      interrupting?
                      at)
                     #:constructor-name event*)

(define (make-event
         type
         details
         interrupting?)
  (event* type details interrupting? (world-elapsed-time (situation-world *situation*))))
   

; increment world time
; return a list of events that occur at new timestamp
(define (advance-time-by-a-jiffy!)
  (define events '())
  (define new-elapsed-time (add1 (world-elapsed-time (situation-world *situation*))))
  (set-world-elapsed-time!
   (situation-world *situation*)
   new-elapsed-time)

  (when (= (modulo (world-elapsed-time (situation-world *situation*)) 100) 0)
    (define suspend-action?
      (eq? (time-of-day-from-jiffies (world-elapsed-time (situation-world *situation*)))
           'night))
    (define ev (make-event 'new-time-of-day (time-of-day-from-jiffies (world-elapsed-time (situation-world *situation*))) suspend-action?))
    (set! events (append-element events ev)))


  (when (not (in-combat?))
    (cond
      ;; Currently, only spawn enemies at daytime
      ((not (eq? (time-of-day-from-jiffies (world-elapsed-time (situation-world *situation*)))
                 'night))
       (define dice-sides 100) ; tweak on a per-location basis
       (define roll (d 1 dice-sides))

       (cond ((= roll 1)
              (define title "Luck roll failure")
              (info-card
               (list (list
                      (string-append " at world time " (number->string (world-elapsed-time (situation-world *situation*))) " ")
                      (string-append " 1d" (number->string dice-sides) " = 1 ")
                      " failure: hostile encounter, spawning enemies "))
               title)
              (define ev
                (make-event 'spawn-enemies
                            '() ; pack info about enemies / event here
                            #t))
              (set! events (append-element events ev))
              (wait-for-confirm)))
       )))
  events
  )



(define (inventory)
  (define actor (situation-pc *situation*))
  
  (define sheet
    (append
     (list
      (list " Item " " Notes "))
     (actor-inventory actor)))
  (info-card
   sheet
   "Inventory"
   )
  #t
  )

; TODO move to state
; currently: quest - status - notes
; and table-display formatted
(define *quests* '())

(provide create-quest)
(define (create-quest quest-symbol)
  (define quest
    (case quest-symbol
      ['pay-off-debt
       (list " pay off the debt to the Collector "
             " in progress "
             " unsettled: 4,328 grams of U-235 ")]
      ['the-anthead
       (list " seek the Anthead Girl "
             " not started "
             " \"not ready yet\" ")]))
  (set! *quests*
        (append-element *quests* quest))

  (info-card
   (list quest)
   "New quest")
  )

(define (quests)
  (define sheet
    (append
     (list
      (list " quest " " status " " notes ")
      )
     *quests*
     ))
  (info-card
   sheet
   "Quests")
  )

(provide current-location)
(define (current-location)
  #;(displayln "-- current-location: TODO move to situation")
  (actor-current-location (situation-pc *situation*)))

(define (clean-up-dead-actor! actor)
  (hash-remove! *enemy-stances* actor)
  (set-location-actors! (current-location) (remove actor (location-actors (current-location))))
  (define corpse (cons 'corpse "Blindscraper corpse"))
  (displayln "clean-up-dead-actor!: todo: add corpse")
  (displayln corpse))

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

(provide actor-in-range?)
(define (actor-in-range? enemy range)
  (define stance (hash-ref *enemy-stances* enemy))
  (eq? (stance-range stance) range))
         

(define (get-next-npc-action actor)
  (case (actor-name actor)
    (["Blindscraper"] (get-blindscraper-action actor))
    (["Grabberkin"] (get-grabberkin-action actor))
    (else (displayln "get-next-npc-action: unknown actor"))))


(define (get-next-action actor)
  (cond ((not (pc-actor? actor)) (get-next-npc-action actor))
        (else
         (serialize-state)
         (get-next-pc-action)))
  )

(define (remove-actor-from-its-current-location! actor)
  (define current-location (actor-current-location actor))
  (when (not (eq? '() current-location))
    (remove-actor-from-location! current-location actor)))

(provide move-actor-to-location!)
(define (move-actor-to-location! actor location)
  (remove-actor-from-its-current-location! actor)
  (set-actor-current-location! actor location)
  (add-actor-to-location! location actor))

; situation
(define (get-current-enemies)
  (filter
   (λ (actor) (and (actor-alive? actor)
                   (not (pc-actor? actor))))
   (location-actors (current-location))))

; situation
(define (get-an-enemy-at-range range)
  (define current-enemies (get-current-enemies))
  (define enemies-shuffled (shuffle current-enemies))
  (define enemy-in-range '())
  (for ([enemy enemies-shuffled])
    (define stance (hash-ref *enemy-stances* enemy '()))
    (when (eq? (stance-range stance) range)
      (set! enemy-in-range enemy)))
  enemy-in-range)

; situation
(define (in-range? target attack-mode)
  (case attack-mode
    ['melee #t]
    [else (displayln "in-range? not implemented yet for this attack mode")]))

(define (get-go-to-text-from-location-to-another from-type to-type)
  (case to-type
    ['ruins "Climb the hill to the ruins."]
    ['swamp "Enter the swamps."] ; TODO: Toggle meta-progression on when the swamps are entered for the first time
    ['edgeflats "Go back to Edgeflats."]
    [else (string-append "Go to " (symbol->string to-type) ".")]))

(define (get-world-choices world actor)
  (cond ((in-combat?)
         (get-combat-choices world actor))
        ((eq? (time-of-day-from-jiffies (world-elapsed-time (situation-world *situation*))) 'night)
         (get-nighttime-choices world actor))
        (else (get-downtime-choices world actor))))

(define (get-combat-choices world actor)
  (define targets (get-current-enemies))

  (define combat-choices '())

  (for ([i (in-range 0 (length targets))])
    (define target (list-ref targets i))
    (define stance (hash-ref *enemy-stances* target))
    (cond ((or (eq? (stance-range stance) 'close)
               (eq? (stance-range stance) 'engaged))
           (define damage-roll (λ () (d 1 2)))
           (define details
             (list
              (cons 'damage-roll damage-roll)
              (cons 'damage-roll-formula "1d2")
              (cons 'damage-type 'bludgeoning)
              ))
           (define choice
             (make-choice
              'attack
              (string-append
               "Attack "
               (get-combatant-name target)
               " in melee with crowbar.")
              (λ ()
                (make-action
                 #:symbol 'melee
                 #:actor (situation-pc *situation*)
                 #:duration 1
                 #:target target
                 #:tags '(initiative-based-resolution)
                 #:details details))))
           (set! combat-choices (append-element combat-choices choice)))
          ))

  (cond ((not (engaged?))
         (define run-choice
           (make-choice
            'flee
            (string-append
             "Run.")
            (λ ()
              (make-action
               #:symbol 'flee
               #:actor (situation-pc *situation*)
               #:duration 1
               #:target '()
               #:tags '(initiative-based-resolution fast)
               #:details '()))))
         (set! combat-choices (append-element combat-choices run-choice))))

  (cond ((and (engaged?)
              (eq? (actor-name (get-an-enemy-at-range 'engaged))
                   "Grabberkin"))
         (define strength-mod (get-attribute-modifier-for (actor-strength actor)))
         (define damage-roll (λ () (d 1 2)))
         (define details
           (list
            (cons 'damage-roll damage-roll)
            (cons 'damage-roll-formula
                  (string-append "1d2 + str mod ["
                                 (number->string strength-mod)
                                 "]"))
            (cons 'damage-type 'bludgeoning)
            ))
         (define break-free-choice
           (make-choice
            'wrestle
            (string-append
             "Try to pull the leg free.")
            (λ ()
              (make-action
               #:symbol 'wrestle
               #:actor (situation-pc *situation*)
               #:duration 1
               #:target (get-an-enemy-at-range 'engaged)
               #:tags '(initiative-based-resolution)
               #:details details))))
         (set! combat-choices (append-element combat-choices break-free-choice))))

  combat-choices
  )

(define (get-nighttime-choices world actor)
  (displayln "get-night-time-choices: TODO not implemented yet")
  (list
   (make-choice
    'sleep
    "Sleep." 
    (λ () (make-action
           #:symbol 'sleep
           #:actor (situation-pc *situation*)
           #:duration 100
           #:target '()
           #:tags '()
           #:details '())))))

(define (get-location-name-from-location-type location-type)
  (cond ((eq? location-type 'swamp) "the swamps")
        (else (string-append "get-location-name-from-location-type: unknown location type: " (symbol->string location-type)))))

(define (get-continue-pending-action-name pending-action)
  (cond ((eq? (action-symbol pending-action) 'go-to-location)
         (string-append
          "Continue towards "
          (get-location-name-from-location-type (location-type (action-target pending-action)))
          "."))
        ((eq? (action-symbol pending-action) 'search-for-paths)
         (string-append
          "Keep on searching for paths."))
        (else (string-append "get-continue-pending-action-name: unknown action symbol: " (symbol->string (action-symbol pending-action))))))

; This should be refactored
; - a big question is, where does much of this logic
; best fit? locations?
; actions.rkt, as in "the grand action table containing possible actions"?
(define (get-downtime-choices world actor)
  (define pending-choices '())
  (when (not (null? *pending-action*))
    (set!
     pending-choices
     (list
      (make-choice
       'go-to-location
       (get-continue-pending-action-name *pending-action*)
       (λ ()
         (begin0
           *pending-action*
           (set! *pending-action* '())))))))
  ;(displayln "PC-not-null")

  
  (define change-location-choices '())
  (define downtime-choices '())
  (when (and (not (in-combat?))
             (not (location-has-tag? (current-location) 'forbid-simple-exit)))
    (cond ((eq? (time-of-day-from-jiffies (world-elapsed-time (situation-world *situation*))) 'night)
           '()))
    (define neighbors
      (location-neighbors (current-location)))
    (for ([i (in-range 0 (length neighbors))])
      (define neighbor (list-ref neighbors i))
      (set! change-location-choices
            (append change-location-choices
                    (list
                     (make-choice
                      'go-to-location
                      (get-go-to-text-from-location-to-another (location-type (current-location)) (location-type neighbor)) 
                      (λ () (make-action
                             #:symbol 'go-to-location
                             #:actor (situation-pc *situation*)
                             #:duration 100
                             #:target neighbor
                             #:tags '(downtime)
                             #:details '())))))))

    (set! downtime-choices
          (if (eq? (location-type (current-location)) 'swamp)
              (list
               (make-choice
                'forage
                (string-append "Forage.")
                (λ () (make-action
                       #:symbol 'forage
                       #:actor (situation-pc *situation*)
                       #:duration 100
                       #:target '()
                       #:tags '(downtime)
                       #:details '()))))
              '())


          ))

  (define end-run-choices '()) ; poor name
  (when (eq? (location-type (current-location)) 'edgeflats)
    (set! end-run-choices
          (list
           (make-choice
            'go-back-to-the-shack
            "Head back to The Shack."
            (λ () (make-action
                   #:symbol 'end-run
                   #:actor (situation-pc *situation*)
                   #:duration 0
                   #:target '()
                   #:tags '(downtime)
                   #:details '()))))))
  (when (eq? (location-type (current-location)) 'spring)
    (set! end-run-choices
          (list
           (make-choice
            'dive-in-spring
            "Dive in the spring."
            (λ () (make-action
                   #:symbol 'win-game
                   #:actor (situation-pc *situation*)
                   #:duration 0
                   #:target '()
                   #:tags '(downtime)
                   #:details '()))))))

  
  (define neighbors
    (location-neighbors (current-location)))

  (define location-specific-choices
    (for/list ([action (location-actions-provided (current-location))])
      (case action
        ['search-for-paths
         (make-choice
          'search-for-paths
          "Search for paths."
          (λ () (make-action
                 #:symbol 'search-for-paths
                 #:actor (situation-pc *situation*)
                 #:duration 100
                 #:target '()
                 #:tags '(downtime)
                 #:details '())))])))
  (define choices-before-pruning
    (append pending-choices change-location-choices downtime-choices end-run-choices location-specific-choices))

  (define (show-choice-based-on-pending-choice? choice)
    (cond ((not (null? pending-choices))
           #f)
          (else
           #t)))
  
  (define
    pruned-choices
    (filter
     show-choice-based-on-pending-choice?
     choices-before-pruning))
  (define all-choices
    (append pending-choices pruned-choices))
  all-choices)

(define (time-of-day-from-jiffies jiffies)
  (define jiffies-of-current-day (remainder jiffies 400))
  (define time-of-day
    (cond ((< jiffies-of-current-day 100) 'morning)
          ((< jiffies-of-current-day 200) 'afternoon)
          ((< jiffies-of-current-day 300) 'evening)
          ((< jiffies-of-current-day 400) 'night)))
  time-of-day
  )

(define action-queue '())
(define (on-begin-round)
  (set-situation-round! *situation* (add1 (situation-round *situation*)))
  (define round-summary
    (list
     (list " round "
           (string-append
            " "
            (number->string (situation-round *situation*))
            " "))
     (list " current location "
           (string-append
            " "
            (symbol->string (location-type (current-location)))
            " "))
     (list " time of day " (string-append " " (symbol->string (time-of-day-from-jiffies (world-elapsed-time (situation-world *situation*)))) " "))
     (list " elapsed time (total) " (string-append " " (number->string (world-elapsed-time (situation-world *situation*))) " "))
     ))
  (info-card round-summary (string-append "Begin round " (number->string (situation-round *situation*))))
  
  (set! action-queue '())
  
  (when (not (null? (situation-current-fragment *situation*)))
    (current-fragment-on-begin-round!))
  )

(define (add-to-action-queue action)
  (set! action-queue (cons action action-queue)))
(define (remove-from-action-queue actions)
  (set! action-queue (remq* actions action-queue)))

(define (sort-action-queue)
  
  (define actions-by-initiatives '())
  (for ([action action-queue])
    (define actor (action-actor action))
    (define dexterity-mod (get-attribute-modifier-for (actor-dexterity actor)))
    
    (define action-mod 0)
    
    (cond ((has-tag? action 'fast)
           (set! action-mod 2))
          ((has-tag? action 'slow)
           (set! action-mod -4)))



    (define dice-1 (d 1 6))
    (define dice-2 (d 1 6))

    (define total (+ dice-1 dice-2 action-mod dexterity-mod))


    (set! actions-by-initiatives (append-element actions-by-initiatives (cons total action))))

  (define shuffled (shuffle actions-by-initiatives)) ; shuffle to avoid sort stability
  (define sorted (sort shuffled
                       (λ (a1 a2) (> (car a1) (car a2))))) ; intentionally flipped: Higher is better

  (define actions
    (for/list ([action-with-initiative sorted])
      (define action (cdr action-with-initiative))
      (define initiative (car action-with-initiative))
      (define action-description
        (string-append
         " "
         (actor-name (action-actor action))
         #;": "
         #;(symbol->string (action-symbol action))
         " "))
      (list action-description (string-append " " (number->string initiative) " "))))
  
  (info-card actions "Action initiatives")

  (set! action-queue '())
  (for ([action-with-initiative sorted])
    (set! action-queue (append-element action-queue (cdr action-with-initiative))))
  
  action-queue)

(define (enqueue-npc-actions)
  (define actors (location-actors (current-location)))
  (for ([actor actors])
    (when (not (pc-actor? actor))
      (define next-action (get-next-action actor))
      (add-to-action-queue next-action))))


; TODO this belongs to AI
(define (update-npc-reactions pc-action)
  (define npcs (get-current-enemies))
  (when (and (aggressive? pc-action)
             (not (in-combat?)))
    ; remove own actions from queue
    (for ([actor npcs])
      (define actions (filter
                       (λ (action) (eq? actor (action-actor action)))
                       action-queue))
      (remove-from-action-queue actions)
      ; blam blam
      #;(define action (make-shoot-action actor))
      (define action '())
      (add-to-action-queue action))
    )
  )


(define (serialize-state)
  ; prng can be stored as vector:
  ; https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._pseudo-random-generator-~3evector%29%29
  '())

(define (serialize-input)
  '())

(define (get-combatant-name actor)
  (cond ((pc-actor? actor)
         "Otava")
        (else
         (define stance (hash-ref! *enemy-stances* actor '()))
         (cond ((= (hash-count *enemy-stances*) 1)
                (append-string (actor-name actor)))
               (else
                (define name (actor-name actor))
                (define index (stance-index stance))
                (append-string name " " index))))))

(define (display-non-pc-combatant-info actor)
  (define stance (hash-ref! *enemy-stances* actor '()))
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
        (list
         " range "
         (string-append " " (symbol->string (stance-range stance)) " "))

        )]))

  (when (not (null? (actor-statuses actor)))
    (define statuses (actor-statuses actor))
    (define statuses-list
      (list " statuses " (~s statuses)))
    (set! body (append-element body statuses-list)))
  (info-card
   body
   name))

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
    (define statuses-list
      (list " statuses "
            (string-append " " (~s statuses) " ")))
    (set! body (append-element body statuses-list)))
  (info-card
   body
   name))

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

(define (describe-situation)
  (cond
    ((in-combat?) (describe-combat-situation)))
  )

(define (redescribe-situation)
  (cond
    ((in-combat?) (describe-combat-situation))
    (else (displayln "redescribe-situation: TODO")))
  )

(define (describe-pc-intention pc-action)
  (case (action-symbol pc-action)
    ['forage (paragraph "Otava is getting low on supplies. Too low to be comfortable. Here looks good as any, so she decides to take a look around, see if there's anything edible.")]
    #;[else (paragraph "TBD")]))

(define (describe-begin-go-to-action action)
  (cond ((eq? 'ruins (location-type (action-target action)))
         "The hillside is steep and slippery.")
        ((eq? 'swamp (location-type (action-target action)))
         "The path soon disappears entirely, and a dense, suffocating fog obscures what little visibility there is through the bushes and thickets of small trees. Here and there are palm-sized patches of asphalt sticking through, fighting overgrown mosses.")
        ("[[begin-go-to description not written yet]")))

(define (describe-finish-go-to-action action)
  (cond ((eq? 'ruins (location-type (action-target action)))
         "Eventually, Otava gets to the top.")
        ((eq? 'swamp (location-type (action-target action)))
         "After a while, Otava finds herself in the middle of the swamps. Through the heavy fog, the bushes swaying in the wind look like evil beast-shadows.")
        ("[[finish-go-to description not written yet]")))

(define (meta-command-valid? meta-commands-with-keys input)
  (set! input (string-upcase input))
  (define meta-command (hash-ref meta-commands-with-keys input '()))
  (if (not (null? meta-command))
      meta-command
      #f))

(define (choice-valid? choices-with-keys input)
  (define choice (hash-ref choices-with-keys (string->number input) '()))
  (if (not (null? choice))
      choice
      #f))

(define (scene-decision-valid? decisions-with-keys input)
  (define decision (hash-ref decisions-with-keys (string->number input) '()))
  (if (not (null? decision))
      decision
      #f))

(define (choice-as-action choices-with-keys input)
  ((choice-resolution-effect (hash-ref choices-with-keys (string->number input) '()))))

(define (handle-fragment-decision decisions-with-keys input)
  (define decision (hash-ref decisions-with-keys (string->number input)))
  (current-fragment-handle-decision! decision))

(define (get-next-pc-action)
  (serialize-state)
  (let/ec produce-action
    (let what-do-you-do ([verbosity 'verbose])
      (define (handle-meta-command meta-commands-with-keys input)
        (set! input (string-upcase input))
        (define meta-command-with-key (hash-ref meta-commands-with-keys input '()))
        (define meta-command (cdr meta-command-with-key))
        (meta-command)
        (redescribe-situation)
        (what-do-you-do 'verbose))
      
      (define actor (situation-pc *situation*))


      (define scene-decisions (if (null? (situation-current-fragment *situation*))
                                  '()
                                  (current-fragment-get-decisions)))
      (define world-choices (get-world-choices (situation-world *situation*) actor))
      
      (define choices (if (null? scene-decisions)
                          world-choices
                          '()))

      (define scene-decisions-with-keys (build-keys-to-choices-map scene-decisions 1))
      (define first-non-scene-index (add1 (length scene-decisions)))
      (define choices-with-keys (build-keys-to-choices-map choices first-non-scene-index)) ; should check for pending actions and name choices accordingly
      (define meta-commands-with-keys (get-meta-commands-with-keys))
      
      (print-choices-and-meta-commands-with-keys choices-with-keys scene-decisions-with-keys meta-commands-with-keys verbosity)
      (define input (wait-for-input))
      (serialize-input)

      (newline)

      (cond ((meta-command-valid? meta-commands-with-keys input) (handle-meta-command meta-commands-with-keys input))
            ((scene-decision-valid? scene-decisions-with-keys input)
             (begin
               (handle-fragment-decision scene-decisions-with-keys input)
               produce-action 'end-round-early))
            ((choice-valid? choices-with-keys input) (produce-action (choice-as-action choices-with-keys input)))
            (else (what-do-you-do 'abbreviated))))))

(provide paragraph)
(define (paragraph . args)
  (displayln (string-append* args))
  (newline))

(define (title)
  (newline)
  (displayln "M A R T A A N V U O")
  (displayln "===================")
  (newline))

(define (print-choices-with-keys choices-with-keys)
  ; TODO: Should order here based on key
  (for ([(k v) (in-hash choices-with-keys)])
    (displayln (string-append "[" (number->string k) "]: " (choice-name v))))
  (newline))

(define (print-decisions-with-keys decisions-with-keys)
  (for ([(k v) (in-hash decisions-with-keys)])
    (displayln (string-append "[" (number->string k) "]: " (decision-title v))))
  #;(newline))
  

(define (key-from-index i)
  (cond ((< i 0) (error "negative index!"))
        ((<= i 8) (add1 i))
        ((= i 9) 0)
        ((> i 9) (error "too many things to do!"))))

(define (build-keys-to-choices-map choices first-index)
  (define choices-with-keys (make-hash))
  (for ([i (in-range (length choices))])
    (define key (key-from-index (+ first-index i -1)))
    (hash-set! choices-with-keys key (list-ref choices i)))
  choices-with-keys)

(define (wait-for-confirm)
  (displayln "[Enter]")
  (newline)
  (define input (read-line))
  input)

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

(define (luck-check)
  (define wangernumbs
    (list
     "i"
     "8-4"
     "21.3"
     ".01"
     "41"
     "9Ɛ"
     "12"
     "70"
     "26"
     "2"
     "-3"
     "±6"))
  (define wanger-index (random (length wangernumbs)))
  (define result (if (< wanger-index (/ (length wangernumbs) 2))
                     #t
                     #f))
  (define result-text (if result
                          " success "
                          " failure "))
  (define wangernumb (list-ref wangernumbs wanger-index))
  (info-card
   (list
    (list " luck " (string-append " " wangernumb " "))
    (list " " result-text))
   "Luck check")
  (wait-for-confirm)
  result)

(provide passive-check)
(define (passive-check type comparator target-number . silent)
  (define text "")
  (case type
    ['charisma-mod (set! text (string-append "charisma mod > " (number->string target-number)))]
    ['fail-charisma-mod (set! text (string-append "fail charisma mod > " (number->string target-number)))]
    [else (error (string-append "passive check: unknown type: " (symbol->string type)))])

  (define attribute-value (get-trait (situation-pc *situation*) "charisma"))
  (define modifier (get-attribute-modifier-for attribute-value))
  (define successful? (> modifier target-number))

  ; dirty but eh: for failures, flip successful here
  (case type
    ['fail-charisma-mod (set! successful? (not successful?))]
    )
  
  (define result (if successful?
                     "check passed"
                     "check failed"))
  (define sheet
    (list
     (list (string-append " " text " ")
           (string-append " " (number->string attribute-value) " (" (get-modifier-string modifier) ") ")
           (string-append " " result " "))
     
     ))
  (when (null? silent)
    (info-card
     sheet
     "Passive check"
     ))
  (wait-for-confirm)
  successful?)

; returns boolean
; (eventually: 'critical-success and 'critical-failure?)
(define (attribute-check title attribute)
  (define roll (d 1 20))
  (define successful? (< roll attribute))
  (define success-string
    (if successful?
        ", success"
        ", failure"))
  (define results
    (list
     (list " 1d20 " " < " " attr ")
     (list
      (string-append
       " "
       (number->string roll))
      " < "
      (string-append " " (number->string attribute) success-string " "))))
               
  (info-card
   results
   (string-append "Attribute check: " title))

  (wait-for-confirm)

  successful?)

; returns boolean
(define (skill-check title bonus target-number)
  (define first-d (d 1 6))
  (define second-d (d 1 6))
  (define roll-total (+ first-d second-d bonus))
  (define successful? (>= roll-total target-number))
  (define success-string
    (if successful?
        ", success"
        ", failure"))
  (define results
    (list
     (list " 2d6 + skill " " >= " " TN ")
     (list
      (string-append
       " "
       (number->string first-d)
       "+"
       (number->string second-d)
       "+"
       (number->string bonus)
       " = "
       (number->string roll-total))
      " >= "
      (string-append " " (number->string target-number) success-string " "))))
               
  (info-card
   results
   (string-append "Skill check: " title))

  (wait-for-confirm)

  successful?)


;;; ACTION RESOLUTION
;;; TODO WHERE DOES THIS GO
(displayln "find me and fix me")
(define (resolve-melee-action! action)
  (define actor (action-actor action))
  (define target (action-target action))
  
  (define target-defense (get-trait target "defense"))

  (define skill (get-trait actor "melee-attack-skill"))

  (define bonus 0)
  
  (cond ((member 'fallen (actor-statuses target))
         (displayln "[Target fallen, TN -2]")
         (set! bonus 2)
         ))
  (cond ((engaged?)
         (displayln "[Engaged, TN +1]")
         (set! bonus -1)
         ))
  
  (define action-target-number (- 7 bonus))

  (define title
    (string-append "Melee, "
                   (get-combatant-name actor)
                   " vs "
                   (get-combatant-name target)))
  (define success? (skill-check title skill action-target-number))

  (define details (action-details action))
  

  (define damage-roll (assoc 'damage-roll details))
  (define damage-roll-formula (cdr (assoc 'damage-roll-formula details)))
  (define damage-roll-result ((cdr damage-roll)))
  

  (when success?
    (info-card
     (list
      (list " damage roll formula " " result ")
      (list
       (string-append " "
                      damage-roll-formula
                      " ")
       (string-append " "
                      (number->string damage-roll-result)
                      " ")))
     "HP damage roll"))

  (define action-result 'ok)
  (when success? (set! action-result (take-damage target damage-roll-result)))
  (when (eq? action-result 'dead)
    
    ; TODO what's a smart place to store this? the actor?
    (case (actor-name (action-target action))
      [("Blindscraper") (award-xp! 7)]))

  (display-combatant-info target)
  (newline)

  action-result
  )

(define (resolve-wrestle-action! action)
  (define actor (action-actor action))
  (define target (action-target action))
  
  #;(define target-defense (get-trait target "defense"))
  (define target-defense (actor-strength target))

  (define skill (get-trait actor "wrestle-attack-skill"))

  (define bonus 0)
  
  (cond ((member 'fallen (actor-statuses target))
         (displayln "[Target fallen, TN -2]")
         (set! bonus 2)
         ))
  
  (define action-target-number (- 7 bonus))

  (define title
    (string-append "Brawl, "
                   (get-combatant-name actor)
                   " vs "
                   (get-combatant-name target)))
  (define success? (skill-check title skill action-target-number))

  (define details (action-details action))
  

  (define damage-roll (assoc 'damage-roll details))
  (define damage-roll-formula (cdr (assoc 'damage-roll-formula details)))
  (define damage-roll-result ((cdr damage-roll)))
  

  (when success?
    (info-card
     (list
      (list " damage roll formula " " result ")
      (list
       (string-append " "
                      damage-roll-formula
                      " ")
       (string-append " "
                      (number->string damage-roll-result)
                      " ")))
     "HP damage roll"))

  (define action-result 'ok)
  (when success? (set! action-result (take-damage target damage-roll-result)))
  (when (eq? action-result 'dead)
    
    ; TODO what's a smart place to store this? the actor?
    (case (actor-name (action-target action))
      [("Blindscraper") (award-xp! 7)]))

  (display-combatant-info target)
  (newline)

  action-result
  )


(define (handle-exploration-check-result! result)
  (if result
      (begin
        (expose-neighbor! (current-location))
        'successful)
      (begin
        (displayln "Exploration failed.")
        'failure)))

(define (resolve-pull-under-action! action)
  (define roll "2d6+whatever")
  (displayln "The gk tries to pull Otava under")
  (displayln roll)
  'ok
  )

(define (resolve-anklebreaker-action! action)
  (define roll "2d6+whatever")
  (displayln "Anklebreaker resolution")
  (displayln roll)
  'ok
  )

(define (resolve-tighten-grip-action! action)
  (define roll "2d6+whatever")
  (displayln "The gk tightens its grip")
  (displayln roll)
  'ok
  )

; can return:
; 'pc-dead  when the pc is dead as a consequence of this action
; 'ok       when the action is completely resolved and not explicitly successful or failed
; 'success  when the action is completely resolved and explicitly successful
; 'failed   when the action is completely resolved and fails
(define (resolve-action! action)
  (when (actor-alive? (action-actor action))
    (cond ((eq? (action-symbol action) 'melee)
           (define result (resolve-melee-action! action))
           (when (eq? result 'dead)
             (if (not (pc-actor? (action-target action)))
                 (paragraph "The " (actor-name (action-target action)) " is dead.")
                 (begin
                   (paragraph "Otava is dead.")
                   'pc-dead))))

          ((eq? (action-symbol action) 'wrestle)
           (define result (resolve-wrestle-action! action))
           (when (eq? result 'dead)
             (if (not (pc-actor? (action-target action)))
                 (paragraph "The " (actor-name (action-target action)) " is dead.")
                 (begin
                   (paragraph "Otava is dead.")
                   'pc-dead))))

          ((eq? (action-symbol action) 'forage)
           (begin
             (define skill 0)
             (define target 8)
             
             (define successful? (skill-check "Forage" skill target))
             
             
             (cond (successful?
                    (define amount (d 1 4)) ; portions = days of survival
                    (define amount-string
                      (if (= amount 1)
                          (string-append (number->string amount) " meal")
                          (string-append (number->string amount) " meals")))

                    (info-card
                     (list
                      (list
                       " 1d4 "
                       " = "
                       (string-append " " amount-string " "))
                      )
                     "Forage results roll")
                    (paragraph "After some time, Otava finds some edible fruits and roots. (" (number->string amount) " meals.)")
                    (define item (list 'food (list amount)))
                    (add-item-to-inventory! (situation-pc *situation*) item)
                    )
                   (else
                    (begin
                      (paragraph "Despite spending a while, Otava can't find anything to eat.")
                      (define luck-roll (d 1 20))
                      (info-card
                       (list
                        (list
                         " 1d20 "
                         " = "
                         (string-append " " (number->string luck-roll) " " )))
                       "Luck roll")
                      )))
             ))
          ((eq? (action-symbol action) 'back-off)
           'ok
           )
          ((eq? (action-symbol action) 'go-to-location)
           'ok
           )
          ((eq? (action-symbol action) 'search-for-paths)
           (define exploration-skill (get-trait (situation-pc *situation*) "exploration-skill"))
           (define target-number 9)
           (define exploration-check-result (skill-check "Exploration" exploration-skill target-number))
           (handle-exploration-check-result! exploration-check-result))
          ((eq? (action-symbol action) 'sleep)
           (paragraph "Otava turns in for the night. Get some rest.")
           'ok)
          ((eq? (action-symbol action) 'flee)
           (cond ((pc-actor? (action-actor action))
                  (paragraph "Otava turns her back to run.")
                  (define skill (get-trait (situation-pc *situation*) "athletics-skill"))

                  (define stance-range-values '())
                  (for ([(k v) (in-hash *enemy-stances*)])
                    (define value (get-stance-range-numeric-value (stance-range v)))
                    (set! stance-range-values (append-element stance-range-values value)))
                  (define target-number
                    ; if there's an enemy in engaged range, then more difficult check
                    (if (member 0 stance-range-values)
                        10
                        8))
           
                  (define success? (skill-check "Athletics" skill target-number))
                  (if success?
                      (begin ; TODO wouldn't it be cool if only failure was explicitly noted :D
                        (paragraph "She dives behind a small bush and waits. Nothing seems to be following her.")
                        (award-xp! 3 "for a working survival instinct")
                        'escape-from-combat)
                      (begin
                        (paragraph "Otava's foot gets caught on a root. She falls face down in the mud.")
                        (actor-add-status! (action-target action) 'fallen 1)
                        (display-pc-combatant-info (situation-pc *situation*))
                        (wait-for-confirm)
                        'failure))
                  )

                 (else ; not a pc actor
                  (paragraph
                   (string-append
                    (get-combatant-name (action-actor action))
                    " tries to run."))
                  (define skill 1)
                  (define stance (hash-ref *enemy-stances* (action-actor action)))
                  (define value (get-stance-range-numeric-value (stance-range stance)))
                  (define target-number
                    (if (= value 0)
                        10
                        8))
           
                  (define success? (skill-check "Athletics" skill target-number))
                  (if success?
                      ; TODO this fails if there are multiple enemies!
                      (begin
                        (paragraph "The Blindscraper skitters away and disappears in the foliage.")
                        (award-xp! 1)
                        'escape-from-combat)
                      (begin
                        (paragraph "It is fast, but not fast enough.")
                        (actor-add-status! (action-actor action) 'fallen 1)
                        (display-combatant-info (action-actor action))
                        'failure))
                  ))
           )

          ; This is starting to get unwieldy... but get poc done first
          ((eq? (action-symbol action) 'go-to-engaged)

           (define lp (pc-actor-lp (situation-pc *situation*)))
           (define dex (actor-dexterity (action-actor action)))
           (define success?
             (cond ((positive? lp)
                    (displayln "[LP positive]")
                    (attribute-check "Dexterity" dex))
                   (else #t)))
           
           (if success?
               (begin
                 (paragraph "The Blindscraper suddenly leaps forward and gets a hold of Otava's forearm with a couple of its lanky fingers. One of its long claws is swinging free, looking for an opening.")
                 (hash-remove! *enemy-stances* (action-actor action))
                 
                 (let ([enemy-stance (stance "α" 'engaged "right")])
                   (hash-set! *enemy-stances* (action-actor action) enemy-stance)))
        
               (begin
                 (paragraph "The Blindscraper leaps at Otava, but she dives under it and stumbles back to her feet.")
                 (displayln "[-1 LP]")
                 (set-pc-actor-lp! (situation-pc *situation*)
                                   (- (pc-actor-lp (situation-pc *situation*))
                                      1))
                 (when (< (pc-actor-lp (situation-pc *situation*)) 0)
                   (set-pc-actor-lp! (situation-pc *situation*)
                                     0))
                 (displayln (pc-actor-lp (situation-pc *situation*)))
                 'failure))
           'ok
           )

          ((eq? (action-symbol action) 'inflict-status)
           (define target (action-target action))
           (match (action-details action)
             ['blind
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
             [(cons 'bound number)
              (paragraph "The Grabberkin tightens its grip around Otava's ankle.")
              (actor-add-status! target 'bound 3)
              ]
             [else (paragraph "todo: unknown status")])
           'ok
           )

          ((eq? (action-symbol action) 'tighten-grip)
           (resolve-tighten-grip-action! action))

          ((eq? (action-symbol action) 'anklebreaker)
           (resolve-anklebreaker-action! action))

          ((eq? (action-symbol action) 'pull-under)
           (resolve-pull-under-action! action))
          
          ((eq? (action-symbol action) 'release-grip)
           'grip-released)

          ((eq? (action-symbol action) 'skip)
           (cond ((member 'silent (action-details action))
                  'ok)
                 (else
                  'ok)))
          
          (else (error (string-append "resolve-action!: unknown action type " (symbol->string (action-symbol action))))))))


(serializable-struct
 timeline
 (metadata
  events
  duration))
 
; breaks on first action-suspending event
; and finishes after duration of jiffies,
; returns a timeline of events that occurred with metadata
(define (advance-time-until-next-interesting-event! jiffies)
  (define metadata '())
  (define events '())
  (define counter 0)
  (let/ec break
    (for ([t jiffies])
      (set! counter (add1 counter))
      (define possible-events-at-t (advance-time-by-a-jiffy!))
      (define events-at-t possible-events-at-t) ; they are real events
      (set! events (append events events-at-t))
        
      ; If any of the events suspends action, then return early
      (define contains-action-suspending-event?
        (memf (λ (event) (event-interrupting? event)) possible-events-at-t))

      ; early-exit
      (when contains-action-suspending-event?
        (set! metadata 'interrupted)
        (break))
      ))
  (timeline metadata events counter))

(provide move-pc-to-location!)
(define (move-pc-to-location! location)
  ; TODO: location on-exit / on-enter triggers here
  #;(displayln (string-append "-- move-pc-to-location!: moving to " (~v location)))
  (remove-actor-from-location! (current-location) (situation-pc *situation*))
  (set-actor-current-location! (situation-pc *situation*) location)
  (add-actor-to-location! location (situation-pc *situation*)))

(define *pending-action* '())

; maybe the event should narrate itself, how about that, hmm?
(define (narrate-event event)
  (case (event-type event)
    ('new-time-of-day
     (case (event-details event)
       ('afternoon (paragraph "It is now afternoon."))
       ('evening (paragraph (string-append
                             "It is evening. "
                             )))
       ('night (paragraph "Night falls. Brutal, pitch-black night."))
       ('morning (paragraph "It is morning."))
       ))
    ; spawn-enemies is complicated to narrate outside of the event itself, so this is faster
    ('spawn-enemies '())
    (else (displayln (string-append "narrate-event: unknown event type "
                                    (symbol->string (event-type event)))))))

; may return:
; void
; 'end-run
; 'win-game
; 'interrupted
; 'success
; 'failure
(provide resolve-pc-action!)
(define (resolve-pc-action! action)
  (define elapsed-time 0)
  (define result (let/ec return
                   ; do these BEFORE action resolution
                   (cond ((eq? (action-symbol action) 'end-run)
                          (define inventory
                            (actor-inventory (situation-pc *situation*)))
                          (displayln inventory)
                          (paragraph "Otava decides to head back to the Shack.")
                          (return 'end-run))
                         ((eq? (action-symbol action) 'win-game)
                          (return 'win-game))
                         ((eq? (action-symbol action) 'go-to-location)
                          (paragraph (describe-begin-go-to-action action))))
                   ; begin advancing time
                   (define timeline
                     (advance-time-until-next-interesting-event! (action-duration action)))
                   (set! elapsed-time (timeline-duration timeline))

                   ; display events
                   (define
                     displayable-events
                     (map
                      (λ (event)
                        (list
                         (string-append " " (number->string (event-at event)) " ")
                         (string-append " " (symbol->string (event-type event)) " ")
                         (string-append " " (~s (event-details event)) " ")
                         (string-append " "
                                        (if (event-interrupting? event)
                                            "yes"
                                            "no")
                                        " ")
                         ))
                      (timeline-events timeline)))
                   #;(info-card
                      (append
                       (list (list " at " " type " " details " " interrupts action? "))
                       displayable-events)
                      (string-append "Timeline, duration " (number->string (timeline-duration timeline))))
                   (for ([event (timeline-events timeline)])
                     (narrate-event event))

                   ; look into https://docs.racket-lang.org/rebellion/Enum_Types.html for enums etc
                   (when (eq? (timeline-metadata timeline) 'interrupted)
                     (handle-pc-action-interrupted! timeline)
                     (return 'interrupted))
    
    
                   ; should check results maybe here?
                   (define action-result (resolve-action! action))
                   
                   ; do these AFTER action resolution
                   (cond ((eq? (action-symbol action) 'go-to-location)
                          (define next-location (action-target action))
                          (move-pc-to-location! next-location)
                          (when (eq? (location-type (current-location)) 'crematory)
                            (go-to-story-fragment 11))
                          (when (eq? (location-type (current-location)) 'swamp)
                            (go-to-story-fragment 20))
                          (paragraph (describe-finish-go-to-action action))))
                   
                   action-result
                   ))

  ; do the state management mutation stuff
  (when (eq? 'interrupted result)
    (define time-left (- (action-duration action) elapsed-time))
    (set! *pending-action* (lens-set action-duration-lens action time-left)))
  result
  )

(define (get-stance-range-numeric-value range)
  (case range
    ['engaged 0]
    ['close 1]
    [else (error "get-stance-range-numeric-value: unknown range")]))

; TODO move to situation
(provide stance)
(serializable-struct
 stance
 (index
  range
  location))

(provide *enemy-stances*)
(define *enemy-stances* (make-hash))

(define (handle-interrupting-event! event)
  (cond ((eq? (event-type event) 'spawn-enemies)
         (define encounter-types '(blindscraper grabberkin))


         ;(define encounter-type 'grabberkin)
         (define encounter-type 'blindscraper)
         (displayln
          (string-append
           "<< setting encounter-type to "
           (symbol->string encounter-type)
           " >>"))

         
         (case encounter-type
           ['grabberkin

            (spawn-grabberkin-encounter!)
            ; TODO this should happen at the end of the encounter for it to make sense narratively
            (set-situation-grabberkin-encounters!
             *situation*
             (add1 (situation-grabberkin-encounters *situation*)))
            (player-info)]
           ['blindscraper
            (spawn-blindscraper-encounter!)
            ]
           )
         )
        (else
         (displayln "handle-interrupting-event!: unknown event type")))
  '())

(define (handle-pc-action-interrupted! timeline)
  (define interrupting-events
    (filter
     (λ (event) (event-interrupting? event))
     (timeline-events timeline)))
  
  (cond ((eq? (length interrupting-events) 1)
         (define event (car interrupting-events))
         (handle-interrupting-event! event)
         )
        (else
         (displayln "handle-pc-action-interrupted!: unexpected amount of interrupting events")))
  )

(define (resolve-npc-action! action)
  (resolve-action! action))

(define (all-actions-of-type? actions type)
  (define predicate
    (λ (action) (eq? (action-symbol action) type)))
  (all-fulfill-predicate? actions predicate))

(define (resolve-turns!)
  (let/ec end-round-early
    (when (all-actions-of-type? action-queue 'flee)
      (paragraph "Otava turns her back to flee and crawls under a bush to hide. She waits a while. Nothing seems to be following her.")
      (award-xp! 1)
      (remove-all-enemies-and-end-combat!)
      (end-round-early))
    (for ([action action-queue])
      (define turn-result (resolve-turn! world action))

      (when (eq? turn-result 'pc-dead) (end-round-early))
      (when (or (eq? turn-result 'escape-from-combat)
                (eq? turn-result 'grip-released)) ; TODO this'll blow up, must handle per opponent
        (remove-all-enemies-and-end-combat!)
        (end-round-early))
      )
    ))

(define (resolve-turn! world action)
  (if (pc-actor? (action-actor action))
      (resolve-pc-action! action)
      (resolve-npc-action! action))
  )



(define (on-end-round)
  (displayln "[End round]")
  (define current-enemies (get-current-enemies))

  (when (= (length current-enemies) 0)
    ; would be nicer to only change when it's currently true, but eh
    (set-situation-in-combat?! *situation* #f))
  
  (when (not (null? (situation-current-fragment *situation*)))
    (current-scene-on-end-round!)) ; TODO scene-rounds should maybe not increase round?

  ; remove statuses
  (for ([enemy (get-current-enemies)])
    (define name (get-combatant-name enemy))
    (when (not (null? (actor-statuses enemy)))
      (displayln (string-append "[" name ": removed statuses:]"))
      (for ([status (actor-statuses enemy)])
        (displayln status))
      (decrement-actor-status-lifetimes! enemy)))

  (for ([enemy (get-current-enemies)])
    (define name (get-combatant-name enemy))
    (when (not (null? (actor-statuses enemy)))
      (define name (get-combatant-name enemy))
      (define description (~s (actor-statuses enemy)))
    
      (define description-prefix
        (string-append "[" name ": removed statuses: "))
      (define description-suffix "]")
      (decrement-actor-status-lifetimes! enemy)))

  ; urgh
  (when (not (null? (actor-statuses (situation-pc *situation*))))
    (define name (get-combatant-name (situation-pc *situation*)))
    (define description (~s (actor-statuses (situation-pc *situation*))))
    
    (define description-prefix
      (string-append "[" name ": removed statuses: "))
    (define description-suffix "]")
    (decrement-actor-status-lifetimes! (situation-pc *situation*)))
  
  
  
  

  (newline) ; This is the "extra" newline that separates rounds
  )

(define (resolve-round)
  (on-begin-round)
  (enqueue-npc-actions)
  (describe-situation)
  
  (serialize-state)
  (let/ec end-round-early-with-round-status
    (define pc-action (get-next-pc-action))
    
    (cond ((eq? pc-action 'end-round-early)
           (on-end-round) ; TODO move on-end-round to the escape continuation where it belongs!
           (end-round-early-with-round-status 'ok))
          (else

           (describe-pc-intention pc-action)
  
           

           (define round-exit-status 'ok)
           (cond ((initiative-based-resolution? pc-action)
                  (add-to-action-queue pc-action)
                  (update-npc-reactions pc-action)
                  (sort-action-queue)
                  (resolve-turns!))
                 (else
                  (define pc-action-result (resolve-pc-action! pc-action))
                  (when (eq? 'end-run pc-action-result) (set! round-exit-status 'end-run))
                  (when (eq? 'win-game pc-action-result) (set! round-exit-status 'win-game))))
           (on-end-round)
           (when (not (actor-alive? (situation-pc *situation*))) (set! round-exit-status 'pc-dead))
           round-exit-status
           ))))

(define (quit)
  (displayln "Really quit? [Q] to quit, anything else to continue.")
  (define input (wait-for-input))
  (set! input (string-upcase input))
  (cond ((equal? input "Q")
         (paragraph "Game exited.")
         (exit))
        (else
         (newline)
         #t))) ; mark input as handled

(define (menu)
  (define (handle-meta-command meta-commands-with-keys input)
    (set! input (string-upcase input))
    (define meta-command-with-key (hash-ref meta-commands-with-keys input '()))
    (define meta-command (cdr meta-command-with-key))
    (meta-command))
  (define (close-menu) #t) ; hacky but eh
  
  (displayln "[Menu]")
  (define meta-commands (make-hash))
  (hash-set! meta-commands "Q" (cons "[Q]: Quit Martaanvuo." quit))
  (hash-set! meta-commands "C" (cons "[C]: Close menu." close-menu))

  (for ([(k v) (in-hash meta-commands)])
    (display (car v))
    (display " "))
  (newline)
  (newline)
  (define input (wait-for-input))
  (serialize-input)

  (newline)

  (cond ((meta-command-valid? meta-commands input) (handle-meta-command meta-commands input))
        (else (menu)))
  #t)

(define (wait-for-input)
  (newline)
  (define input (read-line))
  (newline)
  input)

(define (get-meta-commands-with-keys)
  (define meta-commands (make-hash))
  #;(hash-set! meta-commands "D" (cons "[D]: Describe situation again." describe-situation))
  (hash-set! meta-commands "M" (cons "[M]: Menu." menu))
  (hash-set! meta-commands "C" (cons "[C]: Character sheet." character-sheet))
  (hash-set! meta-commands "I" (cons "[I]: Inventory." inventory))
  (hash-set! meta-commands "Q" (cons "[Q]: Quests." quests))
  meta-commands)

(define (print-meta-commands-with-keys meta-commands-with-keys)
  (for ([(k v) (in-hash meta-commands-with-keys)])
    (display (car v))
    (display " "))
  (newline)
  (newline))


(define (print-choices-and-meta-commands-with-keys choices-with-keys scene-decisions-with-keys meta-commands-with-keys verbosity)
  (cond ((eq? verbosity 'abbreviated)
         (display "Unknown command. Known commands: ")
         (for ([(k v) (in-hash scene-decisions-with-keys)]) (display k))
         (for ([(k v) (in-hash choices-with-keys)]) (display k))
         (for ([(k v) (in-hash meta-commands-with-keys)]) (display k))
         (newline)
         )
        (else
         (newline) ; This is extra spacing, should pass a param to paragraph
         #;(paragraph "What do you do?")
         (print-decisions-with-keys scene-decisions-with-keys)
         (print-choices-with-keys choices-with-keys)
         (print-meta-commands-with-keys meta-commands-with-keys))))

(define (narrate-begin-run)
  (info-card
   (list
    (list " run " (string-append " " (number->string (situation-run *situation*)) " ")))
   (string-append "Begin run number " (number->string (situation-run *situation*))))
  (case (situation-run *situation*)
    [(1)
     (paragraph "After a couple of days of following an old blacktop road, Otava reaches the end of Edgeflats. Before her lie the vast swamplands surrounding Martaanvuo. The Collector told her of a pre-Rains laboratory, abandoned and forgotten. Rumor has it, there's a small reactor in the sub-basement, and with luck, Otava will find enough U-235 to settle her debt to the Collector.")]
    [(2)
     (paragraph "As the path descends, temperature climbs, and Otava soon finds herself drenched in sweat.")]))


(define (on-begin-run)
  (set-situation-run! *situation* (add1 (situation-run *situation*)))
  (set-situation-round! *situation* 0)
  (move-actor-to-location! (situation-pc *situation*) edgeflats)
  (narrate-begin-run)
  (go-to-story-fragment 1)
  )

(define (resolve-a-run)
  (on-begin-run)
  (let/ec end-run
    (let loop ()
      (define round-exit-status (resolve-round))
      (when (eq? round-exit-status 'pc-dead) (end-run 'pc-dead))
      (when (eq? round-exit-status 'win-game) (end-run 'win-game))
      (when (eq? round-exit-status 'end-run) (end-run 'end-run))
      (loop))
    ))

(define (resolve-a-life)
  (on-begin-life)
  (let/ec end-life
    (let loop ()
      (define run-exit-status (resolve-a-run))
      (when (eq? run-exit-status 'pc-dead) (end-life 'pc-dead))
      (when (eq? run-exit-status 'win-game) (end-life 'win-game))
      (when (eq? run-exit-status 'end-run)
        (displayln "TODO: END RUN")
        (loop)))
    ))

(define (player-info)
  (define player-status
    (list
     (list " life " (string-append " " (number->string (situation-life *situation*)) " "))
     (list " grabberkin encounters " (string-append " " (number->string (situation-grabberkin-encounters *situation*)) " "))
     ))
     
  (info-card player-status (string-append "Player status"))
  )

(define (on-begin-life)
  (set-situation-life! *situation* (add1 (situation-life *situation*)))
  (set-situation-pc! *situation* (make-new-pc))
  (player-info)  
  )

(define (on-begin-playthrough)
  ;(paragraph "[" "Begin a story" "]")
  (setup-world)
  )

(define (begin-game)
  #; (random-seed 13)
  (title)
  (on-begin-playthrough)
  (let/ec win-game
    (let begin-new-life ()
      (define pc-life-end-status (resolve-a-life))
      (when (eq? pc-life-end-status 'pc-dead)

        (let end-of-life-menu ([verbosity 'verbose])
          (define (handle-meta-command meta-commands-with-keys input)
            (set! input (string-upcase input))
            (define meta-command-with-key (hash-ref meta-commands-with-keys input '()))
            (define meta-command (cdr meta-command-with-key))
            (meta-command)
            (end-of-life-menu 'verbose))

          (define meta-commands (make-hash))
          (hash-set! meta-commands "Q" (cons "[Q]: Quit." quit))
          (hash-set! meta-commands "R" (cons "[R]: Reincarnate." begin-new-life)) ; TODO: change prompt dynamically based on meta-progress

          (paragraph "Reincarnate?")
          (print-meta-commands-with-keys meta-commands)
          (define input (wait-for-input))
          (serialize-input)

          (newline)

          (cond ((meta-command-valid? meta-commands input) (handle-meta-command meta-commands input))
                (else (end-of-life-menu 'abbreviated)))))
      (when (eq? pc-life-end-status 'win-game) (win-game))))
  (win-game)
  
  )

(define (win-game)
  (paragraph "Otava dives under the waters of Martaanvuo Spring and forever ceases to exist.")
  (wait-for-input)
  (exit))


(begin-game)