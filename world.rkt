#lang racket

(require dyoo-while-loop)

(require "actions.rkt")
(require "actors.rkt")
(require "creatures.rkt")
(require "items.rkt")
(require "locations.rkt")
(require "narration.rkt")
(require "utils.rkt")
(require "pc.rkt")

(define world%
  (class* object% ()
    (field [turn 0]) ; meta
    (field [status 'active]) ; meta - possible values currently 'active and 'ended
    (field [in-combat #f]) ; situational - move outside of class, this is an interpret-the-situation type of function
    (field [elapsed-time 0]) ; jiffies - jiffy is a relative unit that progresses during and between scenes. For downtime, 100 jiffies = 1 TOD

    ; underground: let jiffies drift off from physical needs!

    (define times-of-day '(morning afternoon evening night))
    (define (get-next-time-of-day time-of-day)
      (cond ((eq? time-of-day 'morning) 'afternoon)
            ((eq? time-of-day 'afternoon) 'evening)
            ((eq? time-of-day 'evening) 'night)
            ((eq? time-of-day 'night) 'morning)
            (else error "get-next-time-of-day: not time of day")))

    (field [time-of-day 'midday])
    (field [locations (make-hash)])
    (field [current-location '()]) ; should be defined to (send pc get-current-location)

    (field [pc (new pc%)])

    (field [action-queue '()])

    (super-new)
  
    (define/public (make-connections)
      (begin
        (for ([i (in-range 0 10)])
          (define location (make-location #:index i))
          (hash-set! locations i location))

        (for ([i (in-range 0 10)])
          (define location (hash-ref locations i))
          (define n (random 1 4))

          (for ([j (in-range 0 n)])
            (define next-neighbor-index (random 0 10))
            (define neighbor (hash-ref locations next-neighbor-index))
            (set-field! neighbors
                        location
                        (cons neighbor (get-field neighbors
                                                  location)))))
        (set-field! current-location
                    this
                    (hash-ref locations 0))))
    
    (define/public (get-current-enemies)
      (define all-actors (get-field actors current-location))
      (set! all-actors (remove pc all-actors))
      all-actors)
      

    (define/public (sort-actions!)
      (set! action-queue (sort
                          action-queue
                          action-faster-than)))
    (define/public (clear-action-queue!)
      (set! action-queue '()))

    ; return true if first is less, ie., sorted earlier, than second
    ; ie., #t = action1 is faster than action2
    (define (action-faster-than action1 action2)
      (cond ((has-tag? action1 'slow) #f)
            ((has-tag? action1 'slow) #t)
            ((eq? (action-actor action1) 'pc) #t)
            ((eq? (action-actor action2) 'pc) #f)))))

(define (make-new-world)
  (define world (new world%))
  (send world make-connections)
  (define location (get-field current-location world))
  (send location add-actor! (get-field pc world))
  world)

(define (advance-time! world jiffies)
  (for ([t jiffies])
    (define new-elapsed-time (add1 (get-field elapsed-time world)))
    (set-field! elapsed-time
                world
                new-elapsed-time)))

(define (begin-turn! world)
  (set-field! turn
              world
              (add1 (get-field turn world)))

  (newline)
  (displayln
   (string-append
    "-- Turn " (number->string (get-field turn world))
    ", "
    "elapsed time: " (number->string (get-field elapsed-time world)) " jiffies"
    ", "
    "location: " (number->string (get-field index (get-field current-location world)))
    ", "
    "time of day: " (symbol->string (get-field time-of-day world))
    ))
  (newline))

(define (on-turn! world)
  (cond ((and
          (= (modulo (get-field turn world) 3) 0)
          (not (get-field in-combat world)))
         (begin
           (define challenge-rating (d 1 2))
           (spawn-enemies world challenge-rating)))))

(define (end-turn! world)
  (define pc (get-field pc world))
  (send pc clear-statuses!)

  (define enemies (send world get-current-enemies))
  (when (= (length enemies) 0)
    (set-field! in-combat world #f))
  '())



; This should be reimplemented in terms of describable<%> or something
(define (get-enemies-description enemies)
  (define enemies-in-bins (make-hash))
  (for ([i (in-range 0 (length enemies))])
    (define enemy (list-ref enemies i))
    (define key (send enemy get-name))

    (define similar-enemies (hash-ref enemies-in-bins key '()))
    
    (set! similar-enemies (cons enemy similar-enemies))
    (hash-set! enemies-in-bins key similar-enemies))
  
  (define number-of-keys (length (hash-keys enemies-in-bins)))

  (define (describe-group number-of-enemies name)
    (define name-length (length (string->list name)))
    (define first-character (substring name 0 1))
    (define last-character (substring name (- name-length 2) (- name-length 1)))

    (define (get-indefinite-article name)
      (match first-character
        [(regexp #rx"[aeiouyAEIOUY]") "an"]
        [_ "a"]))
    (define numeral
      (cond ((= number-of-enemies 1) (get-indefinite-article name))
            ((= number-of-enemies 2) "Two")
            ((= number-of-enemies 3) "Three")
            (else "Some")))
    (define inflected-name
      (cond ((= number-of-enemies 1) name)
            (else (match last-character
                    [(regexp #rx"[aeiouyAEIOUY]") (string-append name "s")]
                    [_ (string-append name "es")]))))
    (string-append numeral " " inflected-name))

  (define group-descriptions '())
  (for ([i (in-range 0 number-of-keys)])
    (define key (list-ref (hash-keys enemies-in-bins) i))
    (define enemies-in-bin (hash-ref enemies-in-bins key))
    
    (define number-of-enemies-in-bin (length enemies-in-bin))
    (define group-description (describe-group number-of-enemies-in-bin key))
    (set! group-descriptions (cons group-description group-descriptions)))
  (define enemies-description (get-string-list-inline-description group-descriptions))
  
  enemies-description)

(define (get-enemies-on-spawn-message enemies)
  (define enemies-list-description (get-enemies-description enemies))
  (define on-spawn-message
    (string-append (get-curse) "! " enemies-list-description " suddenly appear!"))
  on-spawn-message)
  

(define (spawn-enemies world challenge-rating)
  (define location (get-field current-location world))
  (define number challenge-rating)
  (define added-enemies '())
  (for ([i (in-range 0 number)])
    (define r (d 1 2))
    (define enemy (cond ((= r 2) (new blindscraper%))
                        (else (new bloodleech%))))
    (set! added-enemies (cons enemy added-enemies))
    
    (send location add-actor! enemy))
  (paragraph (get-enemies-on-spawn-message added-enemies))
  (set-field! in-combat world #t))

(define (player-has-weapons? pc)
  (not (empty? (filter
                (lambda (item) (member 'stab (send item get-uses)))
                (get-field inventory pc)))))

(define (describe-situation world)
  (define in-combat (get-field in-combat world))
  (cond ((not in-combat)
         (displayln (send (get-field current-location world) get-description))
         (newline))
        (in-combat
         (displayln (send (get-field current-location world) get-combat-summary))
         (newline))))

(define (describe-enemy actor index)
  (define
    description
    (string-append
     "#"
     (number->string (add1 index))
     ": "
     (send actor get-name)
     ": "
     (number->string (get-field hp actor))
     " HP."
     ))
  (displayln description))

(define (enemy? actor) ; TODO move elsewhere
  (not (is-a? actor pc%)))

(define (describe-situation-post-on-turn world)
  (cond ((get-field in-combat world)
         (define pc (get-field pc world))
         (displayln (string-append "You are in combat."
                                   " "
                                   (send pc get-status)
                                   " ("
                                   (number->string (get-field hp pc))
                                   " HP)"))
         (newline)
         (define current-location (get-field current-location world))
         (define all-actors (get-field actors current-location))
         (define enemies (filter enemy? all-actors))
         (for ([i (in-range 0 (length enemies))])
           (define enemy (list-ref enemies i))
           (describe-enemy enemy i))
         (newline))))

(define (make-action-from-choice world choice)
  (define action ((choice-resolution-effect choice))) ; check actor
  action)

(define (get-world-choices world actor)
  (define location-choices
    (if (not (get-field in-combat world))
        (send (get-field current-location world)
              get-interaction-choices)
        '()))
  (define next-location-choices
    (if (not (get-field in-combat world))
        (send (get-field current-location world)
              get-exit-choices)
        '()))

  (define combat-choices (send actor get-combat-choices world))
  (define generic-choices (send actor get-generic-choices world))
  (define all-choices (append location-choices next-location-choices combat-choices generic-choices))
  all-choices)

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
       (define loot (send (get-field current-location world) search))
       (cond ((eq? loot '()) (displayln "You find nothing of interest."))
             (else
              (newline)
              (displayln (string-append "Ah ha! You find " (send loot get-inline-description) " half buried under a rock. A gift."))
              (newline)
              (displayln (string-append "You pick up the " (send loot get-short-description) "."))
              (set-field! inventory actor (cons loot (get-field inventory actor)))
              #;(when (is-a? loot sapling-finger%) #;(win) (error "world.rkt: update-state!: Reimplement win!"))))
       (newline)
       (advance-time! world (action-duration action)))]
    ['forage
     (begin
       (define roll (d 2 6))
       (define target 8) ; for woodlands
       (cond ((>= roll target)
              (newline)
              (displayln "You found some food!"))
             (else
              (newline)
              (displayln "You found nothing edible.")))
       (newline)
       (advance-time! world (action-duration action)))]
    ['inventory
     (print-inventory
      (get-list-inline-description
       (get-field inventory actor)))]
    ['go-to-neighboring-location
     (begin
       (newline) ; dunno where to put this
       (send (get-field current-location world) remove-actor! actor) ; hacky
       (send (get-field current-location world) on-exit!)

                                   
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
            (displayln "It doesn't do anything.")
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

(define (wait-for-confirm)
  (newline)
  (displayln "[Enter]")
  (newline)
  (define input (read-line))
  input)

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

(provide (all-defined-out))