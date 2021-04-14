#lang racket

(require racket/serialize)

(require dyoo-while-loop)
(require lens)

(require "action.rkt")
(require "actor.rkt")
(require "creatures.rkt")
(require "items.rkt")
(require "locations.rkt")
(require "narration.rkt")
(require "pc.rkt")
(require "ui.rkt")
(require "utils.rkt")
(require "world.rkt")

; globals and state
;(define *world* '())
(define *metaloop* 0)

#;(define (reset-meta)
    (set! *world* (make-new-world))
    (set! *metaloop* (add1 *metaloop*)))

#;(define (get-next-action actor)
    (cond ((is-a? actor pc%)
           (define choices (get-world-choices *world* actor))
           (define choices-with-keys (build-keys-to-choices-map choices)) ; should check for pending actions and name choices accordingly
           (print-choices-with-keys choices-with-keys)

           (define meta-commands-with-keys (get-meta-commands-with-keys))
           (print-meta-commands-with-keys meta-commands-with-keys)

           (displayln "What do you do?")

           (define input (wait-for-input))

           (newline)

           ; Actually, this should be a while loop:
           ; something like "until-valid-action (get-next-action)"
           (define handled? (try-to-handle-as-meta-command meta-commands-with-keys input))
           (cond (handled? (get-next-action actor))
                 (else
                  (when (not handled?)
                    (set! handled? (try-to-handle-as-choice choices-with-keys input)))
                  (when (not handled?)
                    (set! handled? (pebkac-loop choices-with-keys meta-commands-with-keys)))

                  ; handled? should now contain a valid action
                  (unless handled? (error "Input not handled even in PEBKAC loop!"))

                  (define choice handled?) ; ta-dah

                  (define action (make-action-from-choice *world* choice))
                  (cond ((free? action)
                         (resolve-action! *world* action)
                         (newline)
                         (get-next-action actor)
                         )
                        (else action))))
           )
          (else
           (define action (send actor get-next-action))
           action)))


#;(define (resolve-turn)
    (cond ((eq? (get-field status *world*) 'active)
           (begin-turn! *world*)
           (describe-situation *world*)
           (on-turn! *world*)
           (describe-situation-post-on-turn *world*)
           (define actions '())
           (define current-location (get-field current-location *world*))
           (define actors (location-actors current-location))
  
           (for ([i (in-range (length actors))])
             (define actor (list-ref actors i))
             (define action (get-next-action actor))

             (if (resolve-instantly? action)
                 (resolve-action-instantly! *world* action)
                 (add-action-to-queue *world* action)))

           (sort-actions! *world*)

           (define turn-exit-status (resolve-actions! *world*))
           (cond ((eq? turn-exit-status 'last-chance)
                  (displayln "LAST CHANCE")
                  (displayln "(w/ a hidden saving throw, naturally, but not until when resolving the choice)"))
                 ((eq? turn-exit-status 'pc-dead)
                  (set-field! status *world* 'ended)
                  (newline)
                  (newline)
                  (displayln "YOU ARE DEAD.")
                  (displayln "[[summary goes here]]")
                  (end-game)))

           (end-turn! *world*)
           (resolve-turn))
          ((eq? (get-field status *world*) 'ended)
           (game-ended-loop))))



#;(define (resolve-actions! world)
    (define turn-exit-status 'ok)
    (while (not (empty? (get-field action-queue world)))
           (define action (car (get-field action-queue world)))
         
           (define result (resolve-action! world action))
           (when (not (eq? result 'not-active))
             (wait-for-confirm))

           (cond ((eq? result 'u-ded)
                  (displayln "You die.")
                  (set! turn-exit-status 'pc-dead)
                  (break))
                 ((eq? result 'last-breath)
                  (displayln "You are one hair's breadth from becoming one with the Dark.")
                  (set! turn-exit-status 'last-breath)
                  (send world clear-action-queue!)
                  (break)))
           (set-field! action-queue world
                       (if (pair? (get-field action-queue world))
                           (cdr (get-field action-queue world)) ; pop stack's topmost element
                           '())))
    (when (in-combat? world)
      (advance-time-by-a-jiffy! world)) ; DIRTY HACK but it works for now, eventually consolidate actions and events and turns and jiffies
    turn-exit-status)




(define (sort-actions! world)
  (send world sort-actions!))

(define (add-action-to-queue world action)
  (define new-actions
    (append (get-field action-queue world)
            (list action)))
  (set-field! action-queue world new-actions))

(define (add-pending-action world action elapsed-jiffies)
  (define pending-action (cons (action-symbol action) elapsed-jiffies))
  (define new-pending-actions
    (append (get-field pending-actions world)
            (list pending-action)))
  (set-field! pending-actions world new-pending-actions))



(define (resolve-defensive-attack-action! world action)
  (define actor (action-actor action))
  (define target (action-target action))
  (define location (get-field current-location world))
  (define enemies (send world get-current-enemies))
  
  (when (eq? target 'pc) (set! target (get-field pc world))) ; dirty
  (when (eq? target 'random) (set! target (take-random enemies)))
  (when (eq? actor 'pc) (set! actor (get-field pc world))) ; dirty

  (define attack-skill 1)
  (define target-defense (send target get-current-defense))
  (define attack-roll (+ (d 2 6) 0))
  (define successful? (>= attack-roll target-defense))
  (define attacker-name (send actor get-name))
  (define target-name (send target get-name))

  (displayln
   (string-append
    "-- Defensive attack action: "
    attacker-name
    " attacks "
    target-name))
  (displayln
   (string-append
    "Attack roll: "
    (number->string attack-roll)
    " "
    "against Defense: "
    (number->string target-defense)))
  (cond (successful?
         (define damage (d 1 1))
         (displayln
          (string-append
           "Success, damage: "
           (number->string damage)))
         (define result (send target hit damage))
         (displayln
          (string-append
           "Result: "
           (symbol->string result)))
         (cond ((eq? result 'dead)
                (displayln "ENEMY DEAD")
                (send location remove-actor! target)
                ; TODO: Add enemy corpse
                ))
         result)
        (else
         (displayln "Attack was unsuccessful.")
         'failure)))

(define (resolve-attack-action! world action)
  (define actor (action-actor action))
  (define target (action-target action))
  (define location (get-field current-location world))

  (when (eq? target 'pc)
    (set! target (get-field pc world)))
  (when (eq? actor 'pc)
    (set! actor (get-field pc world)))

  (define attack-skill (get-field attack-skill actor))
  (define target-defense (send target get-current-defense))
  (define attack-roll (+ (d 2 6) attack-skill))
  (define successful? (>= attack-roll target-defense))
  (define attacker-name (send actor get-name))
  (define target-name (send target get-name))

  (displayln
   (string-append
    "-- Attack action: "
    attacker-name
    " attacks "
    target-name))
  (displayln
   (string-append
    "Attack roll: "
    (number->string attack-roll)
    " "
    "against Defense: "
    (number->string target-defense)))
  (cond (successful?
         (define damage (send actor get-brawl-damage))
         (displayln
          (string-append
           "Success, damage: "
           (number->string damage)))
         (define result (send target hit damage))
         (displayln
          (string-append
           "Result: "
           (symbol->string result)))
         (cond ((eq? result 'dead)
                (displayln "ENEMY DEAD")
                (remove-actor-from-location! location target)
                ; TODO: Add enemy corpse
                ))
         result)
        (else
         (displayln "Attack was unsuccessful.")
         'failure)))

(define (resolve-player-action! world action)
  (define actor (get-field pc world)) ; dirty
  (case (action-symbol action)
    ['search
     (begin
       (define roll (d 2 6)) ; "charisma" in the sense of favour of powers would make sense
       (define target 7)
       (when (>= roll target)
         (define loot 'knife)
         (cond ((eq? loot '()) (displayln "You find nothing of interest."))
               (else
                (newline)
                (displayln (string-append "Ah ha! You find a " (symbol->string loot) " sitting on the hand of a corpse. How auspicious!"))
                (newline)
                (displayln (string-append "You pick up the " (symbol->string loot) "."))
                (set-field! inventory actor (cons loot (get-field inventory actor)))
                (when (eq? loot 'sapling-finger) #;(win) (error "world.rkt: update-state!: Reimplement win!"))))
         (newline)))]
    ['forage
     (begin
       (define roll (d 2 6))
       (define target 8)
       (cond ((>= roll target)
              (define amount (d 1 4)) ; portions = days of survival
              (newline)
              (displayln (string-append "You found some edible fruits and roots. (" (number->string amount) " meals)"))
              (newline)
              
              (define loot (make-list amount 'food))
              (set-field! inventory actor (append loot (get-field inventory actor))))
             (else
              (newline)
              (displayln "You found nothing edible.")))
       (newline))]
    ['inventory
     (newline)
     (print-inventory (get-field inventory actor))
     ]
    ['eat
     (newline)
     (displayln "You eat. You are not hungry anymore.")
     (send actor remove-condition! 'hungry)
     (set-field! hunger-counter actor 0)
     (set-field! inventory actor (remove 'food (get-field inventory actor)))
     ]

    ['go-to-neighboring-location
     (begin
       (define current-location (get-field current-location world))
       (remove-actor-from-location! current-location actor)

       (set-pc-location! world actor (action-target action))

       )]
    
    ['defensive-strike
     (define result (resolve-defensive-attack-action! world action))
     result]
    ['brawl
     ; Resolve as attack action
     (define result (resolve-attack-action! world action))
     result
     ]
    
    [else (error (string-append "martaanvuo.rkt: Unknown player action: " (symbol->string (action-symbol action))))]))

(define (resolve-enemy-action! world action)
  (define actor (action-actor action))
  (define actor-alive? (> (get-field hp actor) 0))
  (cond ((not actor-alive?) 'not-active)
        (else
         (case (action-symbol action)
           ['attack
            ; Resolve as attack action
            (define result (resolve-attack-action! world action))
            result
            ]
           ['wait
            (define result null)
            (displayln "The enemy doesn't do anything.")
            result
            ]
           [else (error (string-append "Unknown enemy action: " (symbol->string (action-symbol action))))]))))

#;(define (resolve-action! world action)
    (define actor
      (if (eq? (action-actor action) 'pc)
          (get-field pc world)
          (action-actor action)))
  
    (define result
      (if (is-a? actor pc%)
          (begin
            (resolve-player-action! world action)
            (wait-for-confirm))
          (resolve-enemy-action! world action)))
  
    result)

#;(define (resolve-action-instantly! world action)
    ; a useful place to hack in random events specifically when nothing else is happening
    ; actually, this should be something like "start resolving immediately"
    ; because this is more about queueing

    (define pending-actions (get-field pending-actions world))
    (define match (filter (λ (pending-action)
                            (eq? (car pending-action)
                                 (action-symbol action)))
                          pending-actions))
    (when (not (eq? '() match))
      (define new-time-left (- (cdar match) 1))
      (set! action (lens-set action-duration-lens action new-time-left)))
      
    (define next-event-and-elapsed-time (advance-time-until-next-interesting-event! world (action-duration action)))
    (define next-event (car next-event-and-elapsed-time))
    (define elapsed-time (cdr next-event-and-elapsed-time))
    (define time-left (- (action-duration action) elapsed-time))
    (when (< time-left 0) (error "Error: Time left before action resolved is negative!"))

    (if (not (eq? next-event '()))
        (add-pending-action world action time-left)
        (resolve-action! world action)))




#;(define (end-game)
    (define choices-with-keys (make-hash)) ; TODO not needed
    (define meta-commands-with-keys (make-hash))
    (hash-set! meta-commands-with-keys "Q" (cons "[Q]: Quit." quit))
    (hash-set! meta-commands-with-keys "R" (cons "[R]: Restart." restart))
    ;(print-meta-commands-with-keys meta-commands-with-keys)

    (newline)
    (displayln "[Q] to quit, [R] to restart.")


    (define input (wait-for-input))

    (define handled? (try-to-handle-as-meta-command meta-commands-with-keys input))
    (when (not handled?)
      (set! handled? (pebkac-loop choices-with-keys meta-commands-with-keys)))
    )

#;(define (game-ended-loop)
    (define choices-with-keys (make-hash)) ; TODO not needed
    (define meta-commands-with-keys (make-hash))
    (hash-set! meta-commands-with-keys "Q" (cons "[Q]: Quit." quit))
    (hash-set! meta-commands-with-keys "R" (cons "[R]: Restart." restart))
    ;(print-meta-commands-with-keys meta-commands-with-keys)

    (newline)
    (displayln "[Q] to quit, [R] to restart.")


    (define input (wait-for-input))

    (define handled? (try-to-handle-as-meta-command meta-commands-with-keys input))
    (when (not handled?)
      (set! handled? (pebkac-loop choices-with-keys meta-commands-with-keys)))

    (game-ended-loop))

#;(define (restart) (meta-loop)) ; Dirty, should implement state-machine-like interface for global state transitions

#;(define (meta-loop)
    ;begin new run
    (reset-meta)
    (narrate-run-number *metaloop*)

    (resolve-turn)

    (error "meta-loop: resolve-turn should not exit recursion"))

#;(define (startup)
    (title)
    (narrate-startup)
    (call/ec (end-game (meta-loop))))

#;(startup)





(serializable-struct
 world
 (locations
  day
  time-of-day))

(serializable-struct
 actor
 (name
  hp
  max-hp
  attack-skill
  attack-damage
  defense-number
  inventory
  statuses
  conditions
  [current-location #:mutable]))

(serializable-struct
 pc-actor
 (lp
  max-lp)
 #:super struct:actor)

(define location-1
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:type 'swamp))

(define location-2
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:type 'swamp))

(define *world*
  (world (list location-1 location-2) 0 0))

(define *pc*
  (pc-actor
   "You"
   4
   4
   0
   (λ () (d 1 2))
   6
   '()
   '()
   '()
   '()
   4
   4
   ))

(define (current-location)
  (actor-current-location *pc*))

(define *enemy*
  (actor
   "woman"
   4
   4
   0
   (λ () (d 1 2))
   6
   '()
   '()
   '()
   '()))

(define (get-next-npc-action actor)
  (make-action #:symbol 'hold-at-gunpoint
               #:actor actor
               #:duration 1
               #:target 'pc
               #:tags '(delayed-resolution))
  )


(define (get-next-action actor)
  (cond ((not (pc-actor? actor)) (get-next-npc-action actor)) ; = what do you do?
        (else (get-next-pc-action)))
  )

(define (remove-actor-from-its-current-location! actor)
  (define current-location (actor-current-location actor))
  (when (not (eq? '() current-location))
    (remove-actor-from-location! current-location actor)))

(define (move-actor-to-location! actor location)
  (remove-actor-from-its-current-location! actor)
  (set-actor-current-location! actor location)
  (add-actor-to-location! location actor))

(define (setup-world)
  (move-actor-to-location! *pc* location-1)
  (move-actor-to-location! *enemy* location-1)
  (set-location-neighbors! location-1 (list location-2))
  (set-location-neighbors! location-2 (list location-1))
  )

(define (get-current-enemies)
  (filter
   (λ (actor) (not (pc-actor? actor)))
   (location-actors (current-location))))

(define (get-world-choices world actor)
  (define combat-choices '())
  (define targets (get-current-enemies))
  (for ([i (in-range 0 (length targets))])
    (define target (list-ref targets i))
    (set! combat-choices
          (append combat-choices
                  (list
                   (make-choice
                    'shoot
                    (string-append "Shoot the " (actor-name target) ". (enemy #" (number->string (add1 i)) ")")
                    (λ () (make-action
                           #:symbol 'shoot
                           #:actor *pc*
                           #:duration 1
                           #:target target
                           #:tags '(combat delayed-resolution))))
                   (make-choice
                    'get-closer
                    (string-append "Get closer.")
                    (λ () (make-action
                           #:symbol 'get-closer
                           #:actor *pc*
                           #:duration 1
                           #:target target
                           #:tags '())))
                   (make-choice
                    'explain
                    (string-append "\"I'm just passing through.\"")
                    (λ () (make-action
                           #:symbol 'explain
                           #:actor *pc*
                           #:duration 0
                           #:target target
                           #:tags '(dialogue fast))))
                   (make-choice
                    'barter
                    (string-append "\"I want to barter.\"")
                    (λ () (make-action
                           #:symbol 'barter
                           #:actor *pc*
                           #:duration 0
                           #:target target
                           #:tags '(dialogue fast))))
                   (make-choice
                    'back-off
                    (string-append "Back off.")
                    (λ () (make-action
                           #:symbol 'back-off
                           #:actor *pc*
                           #:duration 1
                           #:target target
                           #:tags '()))))))
    )
  combat-choices
  )

(define action-queue '())
(define (on-begin-round)
  (displayln "on-begin-round")
  (set! action-queue '())
  )

(define (add-to-action-queue action)
  (set! action-queue (cons action action-queue)))
(define (remove-from-action-queue actions)
  (set! action-queue (remq* actions action-queue)))

(define (sort-action-queue)
  (displayln "sort-action-queue")
  )

(define (enqueue-npc-actions)
  (displayln "enqueue-npc-actions")
  (define actors (location-actors (current-location)))
  (for ([actor actors])
    (when (not (pc-actor? actor))
      (define next-action (get-next-action actor))
      (add-to-action-queue next-action)
      (displayln next-action))))

(define (update-npc-reactions pc-action)
  (displayln "update-npc-reactions")
  (when (aggressive? pc-action)
    ; remove own actions from queue
    (for ([actor (get-current-enemies)])
      (define actions (filter
                       (λ (action) (eq? actor (action-actor action)))
                       action-queue))
      (remove-from-action-queue actions))
    ; blam blam
    (define action (make-action
                    #:symbol 'shoot
                    #:actor actor
                    #:duration 1
                    #:target *pc*
                    #:tags '(combat delayed-resolution)))
    (add-to-action-queue action)
    )
  )

(define (serialize-state)
  (displayln "serialize-state")
  )

(define encounter 'scavenger)
(define (describe-situation)
  (displayln "describe-situation")
  (newline)
  (paragraph "\"Stop.\" You hear a harsh voice. \"Not one step closer.\"")
  (paragraph "The voice belongs to a scavenger, looks to be in her forties, gaunt face and tattered clothes. She's aiming a hunting rifle at you.")
  (paragraph "Your revolver is in its holster. You might be able to pull it out in time.")
  )

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

(define (choice-as-action choices-with-keys input)
  ((choice-resolution-effect (hash-ref choices-with-keys (string->number input) '()))))
  

;; this is broken
(define (get-next-pc-action)
  (let/ec produce-action
    (let what-do-you-do ([verbosity 'verbose])
      (define (handle-meta-command meta-commands-with-keys input)
        (set! input (string-upcase input))
        (define meta-command-with-key (hash-ref meta-commands-with-keys input '()))
        (define meta-command (cdr meta-command-with-key))
        (meta-command)
        (what-do-you-do 'verbose))
      (define actor *pc*)

      (define choices (get-world-choices *world* actor))
      (define choices-with-keys (build-keys-to-choices-map choices)) ; should check for pending actions and name choices accordingly
      (define meta-commands-with-keys (get-meta-commands-with-keys))
      (print-choices-and-meta-commands-with-keys choices-with-keys meta-commands-with-keys verbosity)
      (define input (wait-for-input))

      (newline)

      (cond ((meta-command-valid? meta-commands-with-keys input) (handle-meta-command meta-commands-with-keys input))
            ((choice-valid? choices-with-keys input) (produce-action (choice-as-action choices-with-keys input)))
            (else (what-do-you-do 'abbreviated)))
      )
    #;(let loop ()
        (paragraph "What do you do?")
        (define actor *pc*)

        (define choices (get-world-choices *world* actor))
        (define choices-with-keys (build-keys-to-choices-map choices)) ; should check for pending actions and name choices accordingly
        (define meta-commands-with-keys (get-meta-commands-with-keys))


        (print-choices-with-keys choices-with-keys)

    
        (print-meta-commands-with-keys meta-commands-with-keys)


        (define input (wait-for-input))

        (newline)

        (define handled '())
        (while (null? handled)
               (set! handled (try-to-handle-as-meta-command meta-commands-with-keys input))
               (when (not handled)
                 (set! handled (try-to-handle-as-choice choices-with-keys input)))
               (when (not handled)
                 (set! handled (pebkac-loop choices-with-keys meta-commands-with-keys))))

        (when (eq? handled #t) (loop))
        (define choice handled) ; ta-dah
        (displayln "CHOICE:")
        (displayln choice)

        (define action (make-action-from-choice *world* choice))
        (cond ((free? action)
               (resolve-action! *world* action)
               (get-next-action actor))
              (else action)))))

;(define handled? '())
;  (while (null? handled?)
;         (set! handled? (try-to-handle-as-meta-command meta-commands-with-keys input))
;         (when (not handled?)
;           (set! handled? (try-to-handle-as-choice choices-with-keys input)))
;         (when (not handled?)
;           (set! handled? (pebkac-loop choices-with-keys meta-commands-with-keys))))
;  handled?



(define (resolve-action! action)
  (displayln "resolve-action!"))

(define (resolve-pc-action! action)
  (displayln "resolve-pc-action!")
  (resolve-action! action))

(define (resolve-npc-action! action)
  (displayln "resolve-npc-action!")
  (resolve-action! action))

(define (resolve-turns!)
  (displayln "resolve-turns!")
  (for ([action action-queue])
    (resolve-turn! world action))
  )

(define (resolve-turn! world action)
  (displayln "resolve-turn!")
  (displayln action)
  (if (pc-actor? (action-actor action))
      (resolve-pc-action! action)
      (resolve-npc-action! action))
  (newline)
  )

(define (on-end-round)
  (displayln "on-end-round"))

(define (resolve-round)
  (on-begin-round)
  (enqueue-npc-actions)
  (serialize-state)
  (describe-situation)

  (define pc-action (get-next-pc-action))

  (cond ((initiative-based-resolution? pc-action)
         (add-to-action-queue pc-action)
         (update-npc-reactions pc-action)
         (sort-action-queue)
         (resolve-turns!))
        (else
         (resolve-action! *world* pc-action)))
  (on-end-round))

(define (quit)
  (displayln "Really quit? [Q] to quit, anything else to continue.")
  (define input (wait-for-input))
  (set! input (string-upcase input))
  (cond ((equal? input "Q")
         (narrate-quit)
         (exit))
        (else
         (newline)
         #t))) ; mark input as handled

(define (menu)
  (displayln "menu")
  #t)

(define (wait-for-input)
  (newline)
  (define input (read-line))
  input)

(define (get-meta-commands-with-keys)
  (define meta-commands (make-hash))
  (hash-set! meta-commands "Q" (cons "[Q]: Quit." quit))
  (hash-set! meta-commands "D" (cons "[D]: Describe situation again." describe-situation))
  (hash-set! meta-commands "M" (cons "[M]: Menu." menu))
  meta-commands)

(define (print-meta-commands-with-keys meta-commands-with-keys)
  (for ([(k v) (in-hash meta-commands-with-keys)])
    (display (car v))
    (display " "))
  (newline)
  (newline))

(define (run-meta-command meta-command-with-key)
  (define meta-command (cdr meta-command-with-key))
  (meta-command))

(define (print-choices-and-meta-commands-with-keys choices-with-keys meta-commands-with-keys verbosity)
  (cond ((eq? verbosity 'abbreviated)
         (display "Unknown command. Known commands: ")
         (for ([(k v) (in-hash choices-with-keys)]) (display k))
         (for ([(k v) (in-hash meta-commands-with-keys)]) (display k))
         (newline)
         )
        (else
         (paragraph "What do you do?")
         (print-choices-with-keys choices-with-keys)
         (print-meta-commands-with-keys meta-commands-with-keys))))

(define (try-to-handle-as-meta-command valid-meta-commands-with-keys input)
  (set! input (string-upcase input))
  (define meta-command (hash-ref valid-meta-commands-with-keys input '()))
  (if (not (null? meta-command))
      (begin (run-meta-command meta-command)
             #t)
      #f))

(define (try-to-handle-as-choice valid-choices-with-keys input)
  (define choice (hash-ref valid-choices-with-keys (string->number input) '()))
  (if (not (null? choice))
      choice
      #f))


(define (game-loop)
  (let loop ()
    (resolve-round)
    (loop)))

(define (begin-game)
  (title)
  (setup-world)
  (game-loop)
  )

(begin-game)