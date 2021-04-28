#lang racket

(require racket/generator)
(require racket/serialize)

(require dyoo-while-loop)
(require lens)
(require text-table)

(require "action.rkt")
(require "actor.rkt")
(require "location.rkt")
(require "utils.rkt")

(define *life* 0)
(define *run* 0)
(define *round* 0)


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
   #:items '()
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

(define the-spring
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '()
   #:type 'martaanvuo-spring))

(define (setup-world)
  (set-location-neighbors! edges (list crematory ruins))
  (set-location-neighbors! crematory (list edges ruins))
  (set-location-neighbors! ruins (list edges crematory sewers cache))
  (set-location-neighbors! sewers (list ruins workshop))
  (set-location-neighbors! cache (list ruins))
  (set-location-neighbors! workshop (list sewers the-spring))
  (set-location-neighbors! the-spring (list workshop))
  )

(define *world*
  (world (list edges crematory ruins sewers cache workshop the-spring) 0 0))

(define (advance-time-by-a-jiffy!)
  (define events '())
  (define new-elapsed-time (add1 (world-elapsed-time *world*)))
  (set-world-elapsed-time!
   *world*
   new-elapsed-time)

  (when (= (modulo (world-elapsed-time *world*) 100) 0)
    (paragraph (string-append "It is now " (symbol->string (time-of-day-from-jiffies (world-elapsed-time *world*))) ".")))
  #;(when (not in-combat?)
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


(define starting-inventory
  (list
   (list 'bolt-cutters (list 'melee-weapon 'tool))))
(define *pc* '())

(define (character-sheet)
  (define actor *pc*)
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
  (define actor *pc*)
  
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

; TODO shit
(define (get-next-npc-action actor)
  (if (not in-combat?)
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
    ['swamp "Enter the swamp."]
    [else (string-append "Go to " (symbol->string to-type) ".")]))

(define (get-world-choices world actor)
  (define combat-choices '())
  (define targets (get-current-enemies))
  (for ([i (in-range 0 (length targets))])
    (define target (list-ref targets i))
    (when *hacky-in-range?*
      (set! combat-choices
            (append combat-choices
                    (list
                     (make-choice
                      'melee
                      (string-append "Hit the " (actor-name target) " with bolt cutters. (enemy #" (number->string (add1 i)) ")")
                      (λ () (make-action
                             #:symbol 'melee
                             #:actor *pc*
                             #:duration 1
                             #:target target
                             #:tags '(combat delayed-resolution))))))))
    )

  (define change-location-choices '())
  (define downtime-choices '())
  (when (and (not in-combat?)
             (null? current-encounter))
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
                             #:actor *pc*
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
                       #:actor *pc*
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
                   #:actor *pc*
                   #:duration 0
                   #:target '()
                   #:tags '(downtime)))))))
  (when (eq? (location-type (current-location)) 'martaanvuo-spring)
    (set! end-run-choices
          (list
           (make-choice
            'step-in-the-spring
            "Step in the Martaanvuo spring."
            (λ () (make-action
                   #:symbol 'win-game
                   #:actor *pc*
                   #:duration 0
                   #:target '()
                   #:tags '(downtime)))))))

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
  (set! *round* (add1 *round*))
  (define round-summary
    (list
     (list " round "
           (string-append
            " "
            (number->string *round*)
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
     (list " time of day " (string-append " " (symbol->string (time-of-day-from-jiffies (world-elapsed-time *world*))) " "))
     (list " elapsed time (total) " (string-append " " (number->string (world-elapsed-time *world*)) " "))
     ))
  (info-card round-summary (string-append "Begin round " (number->string *round*)))
  
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
  '())

(define (serialize-input)
  '())

(define encounter<%>
  (interface () on-begin-round on-end-round on-get-pc-action get-encounter-choices))

(define *hacky-in-range?* #f)
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
      (cond ((eq? 'melee (action-symbol pc-action))
             (set! current-node 'combat)
             (set! in-combat? #t)
             (paragraph "With a swift motion, Otava grabs her bolt cutters and goes for a swing."))
            ((eq? 'get-closer (action-symbol pc-action))
             (set! *hacky-in-range?* #t)
             (cond ((eq? 'final-warning current-node)
                    (set! current-node 'combat)
                    (paragraph "Otava takes another step. The scavenger squeezes the trigger.")
                    (set! in-combat? #t)
                    (set! current-node 'combat)
                    (update-npc-reactions pc-action)
                    (resolve-turns!))
                   ((eq? 'begin current-node)
                    (define roll (d 1 4))
                    (if (= roll 1)
                        (begin
                          (paragraph "Otava takes a step. The scavenger squeezes the trigger.")
                          (set! in-combat? #t)
                          (set! current-node 'combat)
                          (update-npc-reactions pc-action)
                          (resolve-turns!)
                          )
                        (begin
                          (paragraph "Otava takes a step. The scavenger waves her rifle.")
                          (set! current-node 'final-warning)
                          )))))
            ((eq? 'back-off (action-symbol pc-action))
             (set! current-node 'who-are-you)
             (paragraph "Otava raises her hands above her head. \"I'm unarmed.\""))
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

(define get-next-story-text
  (generator ()
             (yield "The Collector is due for another visit in a couple of days. Heavy clouds hang low in the dreary sky over Otava's head.")
             (yield "Otava might very well be the first human to venture here. Or, the first one after the Rains, at any rate. But on the other hand, there's bound to be something good here, something to settle the debt.")
             '()
             ))

(define get-next-edges-description-text
  (generator ()
             (yield
              (string-append
               "The ground turns wetter with each step as Otava descends towards the swamps. The air is at a standstill, and a musty scent, a wet smell of blooming flowers and rotting wood creeps up her nostrils. The remains of the old blacktop road she's been following disappear, swallowed by the undergrowth. If she's going to go any further, well, this is where she'll step off the road and have to find her own path."))
             (yield
              (string-append
               ""))
             (take-random
              (list
               ""))
             ))

(define get-next-swamp-description-text
  (generator ()
             (yield
              (string-append
               "The whining of mosquitoes is incessant. The stunted, skeletonlike trees have a bare minimum of leaves on them. "
               "Still, with some luck, Otava might find some berries here, be able to make it a couple days longer. "
               "Up ahead to the east, on top of a desolate hill, there's a column of smoke rising from some ruins from before the Rains. It's an arduous climb, maybe half a day to get there."))
             (yield
              (string-append
               "The swamps smell like decay. Blighted trees in various stages of rot. Buzzing of flies. Even here, life finds a way. But so does death. "
               "Up ahead to the east, on top of a desolate hill, there's a column of smoke rising from some ruins from before the Rains. It's an arduous climb, maybe half a day to get there."))
             (take-random
              (list
               "It is stiflingly hot."))
             ))

(define (describe-situation)
  (when (and (not in-combat?)
             (null? current-encounter))
    (define story-text (get-next-story-text))
    (when (not (null? story-text)) (paragraph story-text))

    (cond ((eq? (location-type (current-location)) 'edges)
           (define description-text (get-next-edges-description-text))
           (paragraph description-text)))
    (cond ((eq? (location-type (current-location)) 'swamp)
           (define description-text (get-next-swamp-description-text))
           (paragraph description-text)))

    )

  (when (not (eq? '() current-encounter))
    (case (get-field current-node current-encounter)
      ['begin
       (paragraph "\"Stop.\" Otava hears a harsh voice. \"Not one step closer.\"")
       (paragraph "The voice belongs to a scavenger, looks to be in her forties, gaunt face and tattered clothes. There's a slight limp in her step. She's aiming a hunting rifle at Otava.")
       (paragraph "Otava's bolt cutters are hanging from her belt. She's fast, but not that fast. But maybe if she got a bit closer...")]
      ['barter
       (paragraph "\"You wanna trade? Okay. Let's see what you have, then.\"")
       ]
      ['combat
       (paragraph "Otava is fighting a scavenger.")]
      ['who-are-you
       (paragraph "\"Good. Now, what do you want?\"")]
      ['we-cool
       (paragraph "\"Just passing through, huh? Where to, that ain't my problem unless you make it mine. Got that? So just keep your distance and we're cool.\"")]
      ['final-warning
       (paragraph "\"I said, not one fucking step closer.\"")]
      ))
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

(define (choice-as-action choices-with-keys input)
  ((choice-resolution-effect (hash-ref choices-with-keys (string->number input) '()))))

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
      (serialize-input)

      (newline)

      (cond ((meta-command-valid? meta-commands-with-keys input) (handle-meta-command meta-commands-with-keys input))
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

  (info-card
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
      )))
   "Attack roll")
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

  (info-card
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
      " ")))
   "Target")
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
             (define first-d (d 1 6))
             (define second-d (d 1 6))
             (define skill 0)
             (define roll-total (+ first-d second-d skill))
             (define target 8)
             (define successful? (>= roll-total target))
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
                  (number->string skill)
                  " = "
                  (number->string roll-total))
                 " >= "
                 (string-append " " "8" success-string " "))))
               
             (info-card
              results
              "Forage skill check")
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
                    (add-item-to-inventory! *pc* item)
                    )
                   (else
                    (paragraph "Despite spending a while, Otava can't find anything to eat.")))
             ))
          ((eq? (action-symbol action) 'back-off)
           'ok
           ))))

(define (spawn-encounter)
  (when (eq? '() current-encounter)
    (begin
      (set! current-encounter (new scavenger-encounter%))
      (send current-encounter begin-encounter)
      )))

; TODO: think a bit about how this and resolve-action! work together
(define (resolve-pc-action! action)
  (let/ec return
    (cond ((eq? (action-symbol action) 'go-to-location)
           (define next-location (action-target action))
           (remove-actor-from-location! (current-location) *pc*)
           (set-actor-current-location! *pc* next-location)
           (add-actor-to-location! next-location *pc*)
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
  (set! current-encounter '()))

(define (on-end-round)
  (define current-enemies (get-current-enemies))
  (when (= (length current-enemies) 0)
    (set! in-combat? #f))
  (when (not (eq? '() current-encounter))
    (define encounter-status (send current-encounter on-end-round))
    (when (eq? 'exit-encounter encounter-status) (end-encounter)))

  (newline) ; This is the "extra" newline that separates rounds
  )


(define (resolve-round)
  (on-begin-round)
  (enqueue-npc-actions)
  (describe-situation)
  
  (serialize-state)
  (define pc-action (get-next-pc-action))

  (describe-pc-intention pc-action)
  
  (when (not (eq? '() current-encounter))
    (define encounter-status (send current-encounter on-get-pc-action pc-action))
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
  (when (not (alive? *pc*)) (set! round-exit-status 'pc-dead))
  round-exit-status)

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


(define (print-choices-and-meta-commands-with-keys choices-with-keys meta-commands-with-keys verbosity)
  (cond ((eq? verbosity 'abbreviated)
         (display "Unknown command. Known commands: ")
         (for ([(k v) (in-hash choices-with-keys)]) (display k))
         (for ([(k v) (in-hash meta-commands-with-keys)]) (display k))
         (newline)
         )
        (else
         (newline) ; This is extra spacing, should pass a param to paragraph
         (paragraph "What do you do?")
         (print-choices-with-keys choices-with-keys)
         (print-meta-commands-with-keys meta-commands-with-keys))))

(define (narrate-begin-run)
  (info-card
   (list
    (list " run " (string-append " " (number->string *run*) " ")))
   (string-append "Begin run number " (number->string *run*)))
  (case *run*
    [(1)
     (paragraph "After a couple of days of following a winding path through Fangforest, Otava reaches The Edges – the vast swamplands surrounding Martaanvuo.")]
    [(2)
     (paragraph "As the path descends, temperature climbs, and Otava soon finds herself drenched in sweat.")]))


(define (on-begin-run)
  (set! *run* (add1 *run*))
  (set! *round* 0)
  (move-actor-to-location! *pc* edges)
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

(define (on-begin-life)
  (set! *life* (add1 *life*))
  (set! *pc* (make-new-pc))
  (define life-info
    (list
     (list " life " (string-append " " (number->string *life*) " "))
     ))
     
  (info-card life-info (string-append "Begin life number " (number->string *life*)))
  )

(define (on-begin-story)
  ;(paragraph "[" "Begin a story" "]")
  (setup-world)
  )

(define (begin-game)
  (title)
  (on-begin-story)
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
  (paragraph "Otava steps into Martaanvuo spring and forever ceases to exist.")
  (wait-for-input)
  (exit))

(begin-game)