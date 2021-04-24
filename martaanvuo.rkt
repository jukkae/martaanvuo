#lang racket

(require racket/serialize)

(require dyoo-while-loop)
(require lens)
(require text-table)

(require "action.rkt")
(require "actor.rkt")
(require "location.rkt")
(require "narration.rkt")
(require "ui.rkt")
(require "utils.rkt")

(define *run* 0)
(define *round* 0)


(serializable-struct
 world
 (locations
  day
  time-of-day))

(serializable-struct
 actor
 (name
  [hp #:mutable]
  max-hp
  attack-skill
  attack-damage
  defense-number
  dexterity
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
   "PC"
   4
   4
   0
   (λ () (d 1 2))
   8
   13
   '()
   '()
   '()
   '()
   4
   4
   ))

(define (current-location)
  (actor-current-location *pc*))

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
  (set-location-neighbors! location-1 (list location-2))
  (set-location-neighbors! location-2 (list location-1))
  )

(define (get-current-enemies)
  (filter
   (λ (actor) (and (alive? actor)
                   (not (pc-actor? actor))))
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
                           #:tags '(combat delayed-resolution)))))))
    )

  (define change-location-choices '())
  (when (not in-combat?)
    (set! change-location-choices
          (append change-location-choices
                  (list
                   (make-choice
                    'go-to-next-location
                    (string-append "Go to next location.")
                    (λ () (make-action
                           #:symbol 'go-to-next-location
                           #:actor *pc*
                           #:duration 100
                           #:target '()
                           #:tags '(downtime))))))))
  (append combat-choices change-location-choices)
  )

(define action-queue '())
(define (on-begin-round)
  (set! *round* (add1 *round*))
  (define round-summary
    (list
     (list " round "
           (string-append
            " "
            (number->string *round*)
            " "))
     (list " location "
           (string-append
            " "
            (symbol->string (location-type (current-location)))
            " "))
     ))
  (print-table round-summary)
  (when (and (eq? '() current-encounter)
             (= *round* 2))
    (begin
      (set! current-encounter (new scavenger-encounter%))
      (send current-encounter begin-encounter)
      ))
  (set! action-queue '())
  (when (not (eq? '() current-encounter)) (send current-encounter on-begin-round))
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
                  #:target *pc*
                  #:tags '(combat delayed-resolution)))
  action)

(define (serialize-state)

  '()
  )

(define encounter<%>
  (interface () on-begin-round on-end-round on-get-pc-action get-encounter-choices))

(define scavenger-encounter%
  (class* object% (encounter<%>)
    (field [encounter-nodes '(begin barter who-are-you we-cool final-warning combat)])
    (field [current-node (car encounter-nodes)])
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
      
    
    (define/private (exit-encounter)
      (displayln "The scavenger disappears.")
      (newline)
      (remove-actor-from-its-current-location! scavenger)
      'exit-encounter)

    (define/public (begin-encounter)
      (move-actor-to-location! scavenger (current-location)))

    (define/public (on-begin-round)
      '())

    (define/public (on-end-round)
      (define current-enemies (get-current-enemies))
      (if (= (length current-enemies) 0)
          (exit-encounter)
          '()))

    (define/public (on-get-pc-action pc-action)
      (cond ((eq? 'shoot (action-symbol pc-action))
             (set! current-node 'combat)
             (set! in-combat? #t)
             (paragraph "With a swift motion, you pull out your gun."))
            ((eq? 'get-closer (action-symbol pc-action))
             (cond ((eq? 'final-warning current-node)
                    (set! current-node 'combat)
                    (paragraph "You take another step. She pulls the trigger.")
                    (set! in-combat? #t)
                    (set! current-node 'combat)
                    (update-npc-reactions pc-action)
                    (resolve-turns!))
                   ((eq? 'begin current-node)
                    (define roll (d 1 4))
                    (if (= roll 1)
                        (begin
                          (paragraph "You take a step. She pulls the trigger.")
                          (set! in-combat? #t)
                          (set! current-node 'combat)
                          (update-npc-reactions pc-action)
                          (resolve-turns!)
                          )
                        (begin
                          (paragraph "You take a step. She waves her rifle.")
                          (set! current-node 'final-warning)
                          )))))
            ((eq? 'back-off (action-symbol pc-action))
             (set! current-node 'who-are-you)
             (paragraph "You raise your hands above your head. \"I won't shoot.\""))
            ((eq? 'ask-info (action-symbol pc-action))
             (paragraph "\"You are about three-four days out from Martaanvuo still. There are some caches along the way, if you're lookign to restock. The Anthead Girl is hungry this time of the year.\"")
             (exit-encounter))))

    (define/public (get-encounter-choices)
      (case current-node
        ['begin
         (list
          (make-choice
           'get-closer
           (string-append "Take a step forward.")
           (λ () (make-action
                  #:symbol 'get-closer
                  #:actor *pc*
                  #:duration 1
                  #:target '()
                  #:tags '(aggressive))))
          (make-choice
           'back-off
           (string-append "Back off.")
           (λ () (make-action
                  #:symbol 'back-off
                  #:actor *pc*
                  #:duration 1
                  #:target 'none
                  #:tags '()))))]
        ['barter
         '()
         ]
        ['combat
         '()]
        ['who-are-you
         (list
          (make-choice
           'explain
           (string-append "\"I seek the Anthead Girl of the Riverbank. What can you tell me of the mires ahead?\"")
           (λ () (make-action
                  #:symbol 'ask-info
                  #:actor *pc*
                  #:duration 0
                  #:target '()
                  #:tags '(dialogue fast))))
          (make-choice
           'barter
           (string-append "\"I want to barter.\"")
           (λ () (make-action
                  #:symbol 'barter
                  #:actor *pc*
                  #:duration 0
                  #:target '()
                  #:tags '(dialogue fast)))))]
        ['we-cool
         '()]
        ['final-warning
         (list
          (make-choice
           'get-closer
           (string-append "Take another step forward.")
           (λ () (make-action
                  #:symbol 'get-closer
                  #:actor *pc*
                  #:duration 1
                  #:target '()
                  #:tags '(aggressive))))
          (make-choice
           'back-off
           (string-append "Back off.")
           (λ () (make-action
                  #:symbol 'back-off
                  #:actor *pc*
                  #:duration 1
                  #:target 'none
                  #:tags '()))))]
        
        ))
    ))

(define current-encounter '())

(define in-combat? #f)
(define (describe-situation)
  #;(if in-combat?
        (displayln "[in combat]")
        (displayln "[not in combat]"))
  #;(when (not (eq? '() current-encounter))
      (displayln
       (string-append
        "[current encounter node: "
        (symbol->string (get-field current-node current-encounter))
        "]"))
      (newline))

  (when (not (eq? '() current-encounter))
    (case (get-field current-node current-encounter)
      ['begin
       (paragraph "\"Stop.\" You hear a harsh voice. \"Not one step closer.\"")
       (paragraph "The voice belongs to a scavenger, looks to be in her forties, gaunt face and tattered clothes. There's a slight limp in her step. She's aiming a hunting rifle at you.")
       (paragraph "Your revolver is in its holster. You're fast, but you don't think you're that fast")]
      ['barter
       (paragraph "\"You wanna trade? Okay. Let's see what you have, then.\"")
       ]
      ['combat
       (paragraph "You are in combat with a scavenger.")]
      ['who-are-you
       (paragraph "\"Good. Now, what do you want?\"")]
      ['we-cool
       (paragraph "\"Just passing through, huh? Where to, that ain't my problem unless you make it mine. Got that? So just keep your distance and we're cool.\"")]
      ['final-warning
       (paragraph "\"I said, not one fucking step closer.\"")]
      ))
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

      
      (define world-choices (get-world-choices *world* actor))
      (define encounter-choices (if (eq? current-encounter '())
                                    '()
                                    (send current-encounter get-encounter-choices)))
      
      (define choices (append world-choices encounter-choices))
      
      (define choices-with-keys (build-keys-to-choices-map choices)) ; should check for pending actions and name choices accordingly
      (define meta-commands-with-keys (get-meta-commands-with-keys))
      (print-choices-and-meta-commands-with-keys choices-with-keys meta-commands-with-keys verbosity)
      (define input (wait-for-input))

      (newline)

      (cond ((meta-command-valid? meta-commands-with-keys input) (handle-meta-command meta-commands-with-keys input))
            ((choice-valid? choices-with-keys input) (produce-action (choice-as-action choices-with-keys input)))
            (else (what-do-you-do 'abbreviated))))))


(define (info-card . args)
  (print-table '((a b) (c d)))
  (displayln (string-append* args))
  (newline))

(define (resolve-shoot-action! action)
  (define actor (action-actor action))
  (define target (action-target action))
  (define first-d (d 1 6))
  (define second-d (d 1 6))
  (define attack-bonus (actor-attack-skill actor))
  (define attack-roll (+ first-d second-d))
  (define attack-roll-total (+ first-d second-d attack-bonus))
  (define target-number (actor-defense-number target))
  (define success? (>= attack-roll-total target-number))
  (define success-string (if success? "successful" "failure"))
  (define damage-roll ((actor-attack-damage actor)))

  (displayln "Resolving attack roll:")
  (print-table
   (list
    (list
     (string-append " " (actor-name actor) " ")
     " vs "
     (string-append " " (actor-name target) " "))
    (list " 2d6 + ab " " >= " " defense ")
    (list
     (string-append
      " "
      (number->string first-d)
      "+"
      (number->string second-d)
      "+"
      (number->string (actor-attack-skill actor))
      " = "
      (number->string attack-roll-total)
      " "
      )
     " >= "
     (string-append
      " "
      (number->string target-number)
      ", "
      success-string
      " "
      ))))
  (when success?
    (displayln "Attack successful, HP damage roll:")
    (print-table
     (list
      (list " damage roll formula " " result ")
      (list
       " < see actor > "
       (string-append
        " "
        (number->string damage-roll))
       ))))

  (define action-result 'ok)
  (when success? (set! action-result (take-damage target damage-roll)))

  (displayln "Target:")
  (print-table
   (list
    (list
     (string-append " " (actor-name target) " ")
     "")
    (list
     " hp: "
     (string-append
      " "
      (number->string (actor-hp target))
      "/"
      (number->string (actor-max-hp target))
      " "))))
  (newline)

  action-result
  )

(define (resolve-action! action)
  (when (alive? (action-actor action))
    (cond ((eq? (action-symbol action) 'shoot)
           (define result (resolve-shoot-action! action))
           (when (eq? result 'dead)
             (if (not (pc-actor? (action-target action)))
                 (paragraph "The " (actor-name (action-target action)) " is dead.")
                 (begin
                   (paragraph "You are dead.")
                   'pc-dead))))
          ((eq? (action-symbol action) 'back-off)
           'ok
           ))))

(define (resolve-pc-action! action)
  (resolve-action! action))

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
  (set! current-encounter '()))

(define (on-end-round)
  (define current-enemies (get-current-enemies))
  (when (= (length current-enemies) 0)
    (set! in-combat? #f))
  (when (not (eq? '() current-encounter))
    (define encounter-status (send current-encounter on-end-round))
    (when (eq? 'exit-encounter encounter-status) (end-encounter))))


(define (resolve-round)
  (on-begin-round)
  (enqueue-npc-actions)
  (serialize-state)
  (describe-situation)

  (define pc-action (get-next-pc-action))
  (when (not (eq? '() current-encounter))
    (define encounter-status (send current-encounter on-get-pc-action pc-action))
    (when (eq? 'exit-encounter encounter-status) (end-encounter)))

  (cond ((initiative-based-resolution? pc-action)
         (add-to-action-queue pc-action)
         (update-npc-reactions pc-action)
         (sort-action-queue)
         (resolve-turns!))
        (else
         (resolve-action! pc-action)))
  (on-end-round)
  (define round-exit-status 'ok)
  (when (not (alive? *pc*)) (set! round-exit-status 'pc-dead))
  round-exit-status)

(define (quit)
  (displayln "Really quit? [Q] to quit, anything else to continue.")
  (define input (wait-for-input))
  (set! input (string-upcase input))
  (cond ((equal? input "Q")
         (paragraph "You quit.")
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

(define (game-loop)
  (let/ec pc-dead
    (let loop ()
      (define round-exit-status (resolve-round))
      (when (eq? round-exit-status 'pc-dead) (pc-dead))
      (loop)))
  (displayln "End of game."))

(define (begin-game)
  (title)
  (setup-world)
  (game-loop)
  )

(begin-game)