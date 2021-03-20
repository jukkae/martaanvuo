#lang racket

(require dyoo-while-loop)

(require "actions.rkt")
(require "actors.rkt")
(require "creatures.rkt")
(require "items.rkt")
(require "locations.rkt")
(require "narration.rkt")
(require "pc.rkt")
(require "utils.rkt")
(require "world.rkt")

; globals and state
(define *world* '())
(define *metaloop* 0)

(define (reset-meta)
  (set! *world* (make-new-world))
  (set! *metaloop* (add1 *metaloop*)))

(define (quit)
  (newline)
  (displayln "Really quit? [Q] to quit, anything else to continue.")
  (define input (wait-for-input))
  (set! input (string-upcase input))
  (cond ((equal? input "Q")
         (narrate-quit)
         (exit))
        (else
         (newline)
         #t))) ; mark input as handled

(define (key-from-index i)
  (cond ((< i 0) (error "negative index!"))
        ((<= i 8) (add1 i))
        ((= i 9) 0)
        ((> i 9) (error "too many things to do!"))))

(define (build-keys-to-choices-map choices)
  (define choices-with-keys (make-hash))
  (for ([i (in-range (length choices))])
    (define key (key-from-index i))
    (hash-set! choices-with-keys key (list-ref choices i)))
  choices-with-keys)

(define (get-meta-commands-with-keys)
  (define meta-commands (make-hash))
  (hash-set! meta-commands "Q" (cons "[Q]: Quit." quit))
  meta-commands)

(define (wait-for-input)
  (newline)
  (define input (read-line))
  input)

(define (print-choices-with-keys choices-with-keys)
  (for ([(k v) (in-hash choices-with-keys)])
    (displayln (string-append "[" (number->string k) "]: " (choice-name v))))
  (newline))

(define (print-meta-commands-with-keys meta-commands-with-keys)
  (for ([(k v) (in-hash meta-commands-with-keys)])
    (display (car v))
    (display " "))
  (newline)
  (newline))

(define (run-meta-command meta-command-with-key)
  (define meta-command (cdr meta-command-with-key))
  (meta-command))

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

(define (pebkac-loop choices-with-keys meta-commands-with-keys)
  (display "Unknown command. Known commands: ")
  (for ([(k v) (in-hash choices-with-keys)]) (display k))
  (for ([(k v) (in-hash meta-commands-with-keys)]) (display k))
  (newline)
  
  (define input (wait-for-input))
  (define handled? '())
  (while (null? handled?)
         (set! handled? (try-to-handle-as-meta-command meta-commands-with-keys input))
         (when (not handled?)
           (set! handled? (try-to-handle-as-choice choices-with-keys input)))
         (when (not handled?)
           (set! handled? (pebkac-loop choices-with-keys meta-commands-with-keys))))
  handled?)

(define (get-next-action actor)
  (cond ((is-a? actor pc%)
         (define choices (get-world-choices *world* actor))
         (define choices-with-keys (build-keys-to-choices-map choices))
         (print-choices-with-keys choices-with-keys)

         (define meta-commands-with-keys (get-meta-commands-with-keys))
         (print-meta-commands-with-keys meta-commands-with-keys)

         (displayln "What do you do?")

         (define input (wait-for-input))

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
                (unless handled? (error "Input not handled even in PEBKAC loop!")) ; assert that handled? is truthy - TODO implement assert!

                (define choice handled?) ; ta-dah

                (define action (make-action-from-choice *world* choice))
                (cond ((is-free? action)
                       (resolve-action! *world* action)
                       (newline)
                       (get-next-action actor)
                       )
                      (else action))))
         )
        (else
         (define action (send actor get-next-action))
         action)))


(define (resolve-turn)
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



(define (resolve-actions! world)
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
  turn-exit-status)




(define (sort-actions! world)
  (send world sort-actions!))

(define (add-action-to-queue world action)
  (define new-actions
    (append (get-field action-queue world)
            (list action)))
  (set-field! action-queue world new-actions))



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
  (when (eq? target 'pc) (set! target (get-field pc world))) ; dirty
  (when (eq? actor 'pc) (set! actor (get-field pc world))) ; dirty
  (define attack-skill 1)
  (define target-defense (send target get-current-defense))
  (define attack-roll (+ (d 2 6) 1))
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
         (define damage (d 1 2))
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
         (newline))
       (advance-time! world (action-duration action)))]
    ['forage
     (begin
       (define roll (d 2 6))
       (define target 8) ; for woodlands
       (cond ((>= roll target)
              (define amount (d 1 4))
              (newline)
              (displayln (string-append "You found some food: " (number->string amount) " meals")))
             (else
              (newline)
              (displayln "You found nothing edible.")))
       (newline)
       (advance-time! world (action-duration action)))]
    ['inventory
     (newline)
     (displayln (get-field inventory actor))
     #;(print-inventory
      (get-list-inline-description
       (get-field inventory actor)))]
    ['go-to-neighboring-location
     (begin
       (newline) ; dunno where to put this
       (send (get-field current-location world) remove-actor! actor) ; hacky
       (send (get-field current-location world) on-exit!)

       ; advance-time! should be something like skip-to-the-action!, ie. advance time until something interesting happens, and then bail
       (advance-time! world  (action-duration action))

       (set-field! current-location world (action-target action))
       (send (get-field current-location world) add-actor! actor) ; ungh
       (send (get-field current-location world) on-enter!))]
    ['defensive-strike
     (define result (resolve-defensive-attack-action! world action))
     result]
    ['brawl
     ; Resolve as attack action
     (define result (resolve-attack-action! world action))
     result
     ]
    
    [else (error (string-append "Unknown player action: " (symbol->string (action-symbol action))))]))

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

(define (resolve-action! world action)
  (define actor
    (if (eq? (action-actor action) 'pc)
        (get-field pc world)
        (action-actor action)))
  
  (define result
    (if (is-a? actor pc%)
        (resolve-player-action! world action)
        (resolve-enemy-action! world action)))
  
  result)

(define (resolve-action-instantly! world action)
  ;a useful place to hack in random events specifically when nothing else is happening
  ;(displayln (action-symbol action))
  (resolve-action! world action))




(define (end-game)
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

(define (wait-for-confirm)
  (newline)
  (displayln "[Enter]")
  (newline)
  (define input (read-line))
  input)


(define (game-ended-loop)
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

(define (restart) (meta-loop)) ; Dirty, should implement state-machine-like interface for global state transitions

(define (meta-loop)
  ;begin new run
  (reset-meta)
  (narrate-run-number *metaloop*)

  (resolve-turn)

  (error "meta-loop: resolve-turn should not exit recursion"))

(define (startup)
  (title)
  (narrate-startup)
  (call/cc (end-game (meta-loop))))

(startup)