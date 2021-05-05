#lang racket

(require racket/generator)
(require racket/serialize)

(require lens)
(require text-table)

(require "action.rkt")
(require "actor.rkt")
(require "location.rkt")
(require "utils.rkt")

(serializable-struct
 world
 (locations
  day
  [elapsed-time #:mutable]))

(define edges
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '()
   #:type 'edges))

(define swamp
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()#;'(forbid-simple-exit)
   #:actions-provided '()#;'(search-for-paths)
   #:type 'swamp))

(define crematory
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '()
   #:type 'crematory))

(define ruins
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '()
   #:type 'ruins))

(define sewers
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '()
   #:type 'sewers))

(define cache
  (make-location
   #:actors '()
   #:features '()
   #:items '(veilbreaker-staff)
   #:neighbors '()
   #:tags '()
   #:actions-provided '()
   #:type 'cache))

(define workshop
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '()
   #:type 'workshop))

(define the-cataract
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '(step-under-the-waterfall)
   #:type 'the-cataract))

(define (setup-world)
  (set-location-neighbors! edges (list swamp))
  (set-location-neighbors! swamp (list edges crematory ruins))
  (set-location-neighbors! crematory (list swamp))
  (set-location-neighbors! ruins (list swamp sewers cache))
  (set-location-neighbors! sewers (list ruins workshop))
  (set-location-neighbors! cache (list ruins))
  (set-location-neighbors! workshop (list sewers the-cataract))
  (set-location-neighbors! the-cataract (list workshop))
  )

(define starting-inventory
  (list
   (list 'bolt-cutters (list 'melee-weapon 'tool))))

(define (make-new-pc)
  (pc-actor
   "Otava"
   4
   4
   0
   (λ () (d 1 2))
   8
   13
   starting-inventory
   '()
   '()
   '()
   4
   4
   ))

#;(scene
   (node 1 "Those bolt cutters of yours, looking for some work for them? There's a small cache half a day from here, never touched. Break in, loot all you want, but bring me one thing: A leatherbound book with the inscription 'Yarn of the Devourer of All Things'."
         (decision "Yarn of the what?" (go-to-node 2))
         )
   
   (node 2 "'Yarn of the Devourer of All Things'. It's, uh, it's a mythological book, worthless really, but of historical interest to us. To me. Walk in, walk out, you get to keep whatever you find, except for the book. What do you say?"
         (decision "Yeah, sounds like a great opportunity actually." (go-to-node 3))
         (decision "'Us'? Who's us?" (go-to-node 4))
         (decision #:condition (charisma-check?) "Don't you typically pay someone to do a job for you?" (go-to-node 5))
         (decision "Not interested." (end-scene)))
   
   (node 3 "Great! So, ... – he tells you the route: down the path, right from the rock, climb the hill, on the other side there's a fence. I'll meet you here tomorrow." (add-quest 'fetch-the-book) (end-scene))
   )

(define scene<%>
  (interface () on-begin-round! get-scene-decisions handle-scene-decision! on-end-round!))

(define opening-scene%
  (class* object% (scene<%>)
    (super-new)
    (define/public (on-begin-round!)
      (displayln "-- (on-begin-round!)"))
    (define/public (get-scene-decisions)
      (displayln "-- (get-scene-decisions): TODO")
      (list (make-choice 'test "scene decision name" '())))
    (define/public (handle-scene-decision! scene-decision)
      (displayln "-- (handle-scene-decision!)"))
    (define/public (on-end-round!)
      (displayln "-- (on-end-round!)"))))

(define opening-scene (new opening-scene%))

(serializable-struct
 situation
 (world
  [pc #:mutable]
  [life #:mutable]
  [run #:mutable]
  [round #:mutable]
  [elapsed-time #:mutable]
  [in-combat? #:mutable]
  [current-scene #:mutable]))

(define *situation*
  (let ([new-world (world (list edges crematory ruins sewers cache workshop the-cataract) 0 0)]
        [pc (make-new-pc)])
    (situation new-world pc 0 0 0 0 #f opening-scene)))

(define (in-combat?)
  (displayln "-- in-combat? TODO fix")
  (situation-in-combat? *situation*))

(define (advance-time-by-a-jiffy!)
  (define events '())
  (define new-elapsed-time (add1 (world-elapsed-time (situation-world *situation*))))
  (set-world-elapsed-time!
   (situation-world *situation*)
   new-elapsed-time)

  (when (= (modulo (world-elapsed-time (situation-world *situation*)) 100) 0)
    (paragraph (string-append "It is now " (symbol->string (time-of-day-from-jiffies (world-elapsed-time (situation-world *situation*)))) ".")))
  #;(when (not (in-combat?))
      (cond ((not (eq? current-time-of-day 'night))
             (define roll (d 1 200))
             (cond ((= roll 1)
                    (spawn-enemies world 2)
                    (wait-for-confirm)
                    (set! events (cons 'enemies-spawned events)))))
            ((eq? current-time-of-day 'night)
             (define dice-sides (if (or (eq? (get-field current-location world) 'tunnel)
                                        (eq? (get-field current-location world) 'ruins))
                                    1000 ; indoors locations are safer
                                    100))
             (define roll (d 1 dice-sides))
             (cond ((= roll 1)
                    (spawn-enemies world 3)
                    (wait-for-confirm)
                    (set! events (cons 'enemies-spawned events))))
             )))
  events
  )


(define (advance-time-until-next-interesting-event! jiffies)
  (let/ec return
    (for ([t jiffies])
      (define event (advance-time-by-a-jiffy!))
      (when (not (eq? event '()))
        (return (cons event t))))
    (cons '() jiffies)))


(define (character-sheet)
  (define actor (situation-pc *situation*))
  (define sheet
    (list
     (list " Name " (string-append " " (actor-name actor) " "))
     (list " HP " (string-append " " (number->string (actor-hp actor)) "/" (number->string (actor-max-hp actor)) " "))
     (list " Dexterity " (string-append " " (number->string (actor-dexterity actor)) " "))
     (list " Attack skill " (string-append " " (number->string (actor-attack-skill actor)) " "))
     ))
  (info-card
   sheet
   "Character sheet"
   )
  #t
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

(define (current-location)
  #;(displayln "-- current-location: TODO move to situation")
  (actor-current-location (situation-pc *situation*)))

(define (take-damage actor damage)
  (when (< damage 0) (error "take-damage: damage cannot be less than 0"))
  (define new-hp (- (actor-hp actor) damage))
  (when (< new-hp 0) (set! new-hp 0))
  (set-actor-hp! actor new-hp)
  (if (= 0 (actor-hp actor))
      'dead
      'hit))

(define (alive? actor)
  (> (actor-hp actor) 0))

; TODO shit
(define (get-next-npc-action actor)
  (if (not (in-combat?))
      (make-action #:symbol 'hold-at-gunpoint
                   #:actor actor
                   #:duration 1
                   #:target 'pc
                   #:tags '(delayed-resolution))
      (make-shoot-action actor))
  
  )


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

(define (move-actor-to-location! actor location)
  (remove-actor-from-its-current-location! actor)
  (set-actor-current-location! actor location)
  (add-actor-to-location! location actor))

(define (get-current-enemies)
  (filter
   (λ (actor) (and (alive? actor)
                   (not (pc-actor? actor))))
   (location-actors (current-location))))

(define (in-range? target attack-mode)
  (case attack-mode
    ['melee #t]
    [else (displayln "in-range? not implemented yet for this attack mode")]))

(define (get-go-to-text-from-location-to-another from-type to-type)
  (case to-type
    ['ruins "Climb the hill to the ruins."]
    ['swamp "Enter the swamps."]
    [else (string-append "Go to " (symbol->string to-type) ".")]))

(define (get-world-choices world actor)
  (define combat-choices '())
  (define targets (get-current-enemies))
  (for ([i (in-range 0 (length targets))])
    (define target (list-ref targets i))
    '()
    )

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
                             #:tags '(downtime))))))))

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
                       #:tags '(downtime)))))
              '())


          ))

  (define end-run-choices '()) ; poor name
  (when (eq? (location-type (current-location)) 'edges)
    (set! end-run-choices
          (list
           (make-choice
            'go-back-to-the-shack
            "Head back to the shack."
            (λ () (make-action
                   #:symbol 'end-run
                   #:actor (situation-pc *situation*)
                   #:duration 0
                   #:target '()
                   #:tags '(downtime)))))))
  (when (eq? (location-type (current-location)) 'the-cataract)
    (set! end-run-choices
          (list
           (make-choice
            'step-under-the-waterfall
            "Step under Martaanvuo Cataract."
            (λ () (make-action
                   #:symbol 'win-game
                   #:actor (situation-pc *situation*)
                   #:duration 0
                   #:target '()
                   #:tags '(downtime)))))))

  (when (and (eq? (location-type (current-location)) 'swamp)
             (not (in-combat?))
             )
    (define neighbors
      (location-neighbors (current-location)))
    '()
    )

  (append combat-choices change-location-choices downtime-choices end-run-choices)
  )

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
     (list " current location id "
           (string-append
            " "
            (number->string (location-id (current-location)))
            " "))
     (list " time of day " (string-append " " (symbol->string (time-of-day-from-jiffies (world-elapsed-time (situation-world *situation*)))) " "))
     (list " elapsed time (total) " (string-append " " (number->string (world-elapsed-time (situation-world *situation*))) " "))
     ))
  (info-card round-summary (string-append "Begin round " (number->string (situation-round *situation*))))
  
  (set! action-queue '())
  #; (when (not (eq? '() current-encounter)) (send current-encounter on-begin-round!))
  (when (not (null? (situation-current-scene *situation*)))
    (send (situation-current-scene *situation*) on-begin-round!))
  )

(define (add-to-action-queue action)
  (set! action-queue (cons action action-queue)))
(define (remove-from-action-queue actions)
  (set! action-queue (remq* actions action-queue)))

(define (sort-action-queue)
  (set! action-queue (sort
                      action-queue
                      action-faster-than?))
  
  )

(define (enqueue-npc-actions)
  (define actors (location-actors (current-location)))
  (for ([actor actors])
    (when (not (pc-actor? actor))
      (define next-action (get-next-action actor))
      (add-to-action-queue next-action))))


(define (replace-own-action-in-action-queue-with-new-action action)
  (define actions (filter
                   (λ (action) (eq? actor (action-actor action)))
                   action-queue))
  (remove-from-action-queue actions)
  (add-to-action-queue action)
  )

(define (update-npc-reactions pc-action)
  (define npcs (get-current-enemies))
  (when (aggressive? pc-action)
    ; remove own actions from queue
    (for ([actor npcs])
      (define actions (filter
                       (λ (action) (eq? actor (action-actor action)))
                       action-queue))
      (remove-from-action-queue actions)
      ; blam blam
      (define action (make-shoot-action actor))
      (add-to-action-queue action))
    )
  )

(define (make-shoot-action actor)
  (define action (make-action
                  #:symbol 'shoot
                  #:actor actor
                  #:duration 1
                  #:target (situation-pc *situation*)
                  #:tags '(combat delayed-resolution)))
  action)

(define (serialize-state)
  '())

(define (serialize-input)
  '())

(define encounter<%>
  (interface () on-begin-round! on-end-round! on-get-pc-action! get-encounter-choices))

(define scavenger-encounter%
  (class* object% (encounter<%>)
    (field [encounter-nodes '()])
    (field [current-node '()#;(car encounter-nodes)])
    (super-new)

    (define scavenger
      (actor
       "scavenger"
       4
       4
       2
       (λ () (d 1 3))
       8
       7
       '()
       '()
       '()
       '()))
      
    
    (define/private (exit-encounter!)
      (displayln "The scavenger disappears.")
      (newline)
      (remove-actor-from-its-current-location! scavenger)
      'exit-encounter)

    (define/public (begin-encounter!)
      (move-actor-to-location! scavenger (current-location)))

    (define/public (on-begin-round!)
      '())

    (define/public (on-end-round!)
      (define current-enemies (get-current-enemies))
      (if (= (length current-enemies) 0)
          (exit-encounter!)
          '()))

    (define/public (on-get-pc-action! pc-action)
      '())

    (define/public (get-encounter-choices)
      '())
    ))

(define (describe-situation)
  '()
  )

(define (describe-pc-intention pc-action)
  (case (action-symbol pc-action)
    ['forage (paragraph "Otava is getting low on supplies. Too low to be comfortable. Here looks good as any, so she decides to take a look around, see if there's anything edible.")]
    #;[else (paragraph "TBD")]))

(define (describe-go-to-action action)
  (cond ((eq? 'ruins (location-type (action-target action)))
         "The hillside is steep and slippery. Otava slips a couple of times on her way up, but eventually gets to the top.")
        ("[[go-to description not written yet]")))

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

(define (handle-scene-decision scene-decisions-with-keys input)
  (displayln "-- handle-scene-decision: TODO")
  (define decision (hash-ref scene-decisions-with-keys (string->number input)))
  (send (situation-current-scene *situation*) handle-scene-decision! decision))

(define (get-next-pc-action)
  (serialize-state)
  (let/ec produce-action
    (let what-do-you-do ([verbosity 'verbose])
      (define (handle-meta-command meta-commands-with-keys input)
        (set! input (string-upcase input))
        (define meta-command-with-key (hash-ref meta-commands-with-keys input '()))
        (define meta-command (cdr meta-command-with-key))
        (meta-command)
        (what-do-you-do 'verbose))
      
      (define actor (situation-pc *situation*))


      (define scene-decisions (if (null? (situation-current-scene *situation*))
                                  '()
                                  (send (situation-current-scene *situation*) get-scene-decisions)))
      (define world-choices (get-world-choices (situation-world *situation*) actor))
      #;(define encounter-choices (if (null? current-encounter)
                                      '()
                                      (send current-encounter get-encounter-choices)))
      #;(define choices (append world-choices encounter-choices))
      (define choices world-choices)

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
               (handle-scene-decision scene-decisions-with-keys input)
               produce-action 'end-round-early))
            ((choice-valid? choices-with-keys input) (produce-action (choice-as-action choices-with-keys input)))
            (else (what-do-you-do 'abbreviated))))))

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
    (displayln (string-append "[" (number->string k) "]: " (choice-name v))))
  (newline))
  

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
  (newline)
  (displayln "[Enter]")
  (newline)
  (define input (read-line))
  input)

(define (info-card content title)
  (when (not (null? title)) (displayln (string-append "[" title "]")))
  (print-table content #:row-sep? #f)
  (newline))


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
     (list " 2d6 + skill " " >= " " location TN ")
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
      (string-append " " "8" success-string " "))))
               
  (info-card
   results
   (string-append "Skill check: " title))

  successful?)

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

(define (resolve-shoot-action! action)
  (define actor (action-actor action))
  (define target (action-target action))
  
  (define target-number (actor-defense-number target))
  
  (define success? (skill-check "Shoot" (actor-attack-skill actor) target-number))
  (define damage-roll ((actor-attack-damage actor)))

  (when success?
    (info-card
     (list
      (list " damage roll formula " " result ")
      (list
       " < see actor > "
       (string-append
        " "
        (number->string damage-roll))
       ))
     "HP damage roll"))

  (define action-result 'ok)
  (when success? (set! action-result (take-damage target damage-roll)))

  (actor-status-card target (actor-name target))
  (newline)

  action-result
  )

(define (resolve-action! action)
  (when (alive? (action-actor action))
    (cond ((or (eq? (action-symbol action) 'shoot)
               (eq? (action-symbol action) 'melee))
           (define result (resolve-shoot-action! action))
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
           ))))

(define (spawn-encounter)
  (displayln "-- spawn-encounter disabled")
  #;(when (eq? '() current-encounter)
      (begin
        (set! current-encounter (new scavenger-encounter%))
        (send current-encounter begin-encounter!)
        )))

; TODO: think a bit about how this and resolve-action! work together
(define (resolve-pc-action! action)
  (let/ec return
    (cond ((eq? (action-symbol action) 'go-to-location)
           (define next-location (action-target action))
           (remove-actor-from-location! (current-location) (situation-pc *situation*))
           (set-actor-current-location! (situation-pc *situation*) next-location)
           (add-actor-to-location! next-location (situation-pc *situation*))
           (when (eq? (location-type (current-location)) 'ruins)
             (spawn-encounter))
           (paragraph (describe-go-to-action action))
           )
          ((eq? (action-symbol action) 'end-run)
           (return 'end-run))
          ((eq? (action-symbol action) 'win-game)
           (return 'win-game)))
    (resolve-action! action)
    (advance-time-until-next-interesting-event! (action-duration action)) ; TODO note that this might return an event
    )
  )

(define (resolve-npc-action! action)
  (resolve-action! action))

(define (resolve-turns!)
  (let/ec end-round-early
    (for ([action action-queue])
      (define turn-result (resolve-turn! world action))
      (when (eq? turn-result 'pc-dead) (end-round-early))
      )
    ))

(define (resolve-turn! world action)
  (if (pc-actor? (action-actor action))
      (resolve-pc-action! action)
      (resolve-npc-action! action))
  )

(define (end-encounter)
  (displayln "-- end-encounter disabled")
  #;(set! current-encounter '()))

(define (on-end-round)
  (define current-enemies (get-current-enemies))
  (when (= (length current-enemies) 0)
    (displayln "-- on-end-round: fix (in-combat?)")
    #;(set! in-combat? #f))
  (when (not (null? (situation-current-scene *situation*)))
    (send (situation-current-scene *situation*) on-end-round!))
  #;(when (not (eq? '() current-encounter))
      (define encounter-status (send current-encounter on-end-round!))
      (when (eq? 'exit-encounter encounter-status) (end-encounter)))

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
  
           #;(when (not (null? current-encounter))
               (define encounter-status (send current-encounter on-get-pc-action! pc-action))
               (when (eq? 'exit-encounter encounter-status) (end-encounter)))

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
           (when (not (alive? (situation-pc *situation*))) (set! round-exit-status 'pc-dead))
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
  (displayln "menu")
  #t)

(define (wait-for-input)
  (define input (read-line))
  (newline)
  input)

(define (get-meta-commands-with-keys)
  (define meta-commands (make-hash))
  (hash-set! meta-commands "Q" (cons "[Q]: Quit." quit))
  (hash-set! meta-commands "D" (cons "[D]: Describe situation again." describe-situation))
  (hash-set! meta-commands "M" (cons "[M]: Menu." menu))
  (hash-set! meta-commands "C" (cons "[C]: Character sheet." character-sheet))
  (hash-set! meta-commands "I" (cons "[I]: Inventory." inventory))
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
         (for ([(k v) (in-hash choices-with-keys)]) (display k))
         (for ([(k v) (in-hash meta-commands-with-keys)]) (display k))
         (newline)
         )
        (else
         (newline) ; This is extra spacing, should pass a param to paragraph
         (paragraph "What do you do?")
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
     (paragraph "After a couple of days of following a winding path through Fangforest, Otava reaches The Edges – the vast swamplands surrounding Martaanvuo.")]
    [(2)
     (paragraph "As the path descends, temperature climbs, and Otava soon finds herself drenched in sweat.")]))


(define (on-begin-run)
  (set-situation-run! *situation* (add1 (situation-run *situation*)))
  (set-situation-round! *situation* 0)
  (move-actor-to-location! (situation-pc *situation*) edges)
  (narrate-begin-run))

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

(define (on-begin-life)
  (set-situation-life! *situation* (add1 (situation-life *situation*)))
  (set-situation-pc! *situation* (make-new-pc))
  (define life-info
    (list
     (list " life " (string-append " " (number->string (situation-life *situation*)) " "))
     ))
     
  (info-card life-info (string-append "Begin life number " (number->string (situation-life *situation*))))
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
  (paragraph "Otava steps under the roaring waters of Martaanvuo Cataract and forever ceases to exist.")
  (wait-for-input)
  (exit))

(begin-game)