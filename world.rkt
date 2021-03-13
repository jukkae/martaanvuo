#lang racket

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
    (field [in-combat #f]) ; situational
    (field [elapsed-time 0]) ; jiffies
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
      all-actors)))

(define (make-new-world)
  (define world (new world%))
  (send world make-connections)
  (define location (get-field current-location world))
  (send location add-actor! (get-field pc world))
  world)

(define (advance-time! world jiffies)
  (define new-elapsed-time (+ (get-field elapsed-time world)
                              jiffies))
  (set-field! elapsed-time
              world
              new-elapsed-time))

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
         (begin (spawn-enemies world 3)))))

(define (resolve-actions! world)
  (map (λ (action)
         (resolve-action! world action))
       (get-field action-queue world)))

(define (end-turn! world)
  #;(displayln (append-string "-- *world* : end-turn!")) '())



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
  

(define (spawn-enemies world number)
  (define location (get-field current-location world))
  (define added-enemies '())
  (for ([i (in-range 0 number)])
    (define enemy (cond ((= i 2) (new blindscraper%))
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
  (displayln (send (get-field current-location world) get-description))
  (newline))

(define (describe-actor actor)
  (define
    description
    (string-append
     (send actor get-name)
     ": "
     (number->string (get-field hp actor))
     " HP."
     ))
  (displayln description))

(define (describe-situation-post-on-turn world)
  (cond ((get-field in-combat world)
         (displayln "You are in combat.")
         (define current-location (get-field current-location world))
         (define all-actors (get-field actors current-location))
         (define enemies all-actors)
         (map (λ (enemy) (describe-actor enemy))
              enemies)
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

(define (resolve-attack-action! world action)
  (define actor (action-actor action))
  (define target (action-target action))
  "not implemented")
  

(define (resolve-player-action! world action)
  (define actor (get-field pc world)) ; dirty
  (case (action-symbol action)
    ['search
     (begin
       (define loot (send (get-field current-location world) search))
       (cond ((eq? loot 'nothing) (displayln "You find nothing of interest."))
             (else
              (newline)
              (displayln (string-append "Ah ha! You find " (send loot get-inline-description) " half buried under a rock. A gift."))
              (newline)
              (displayln (string-append "You pick up the " (send loot get-short-description) "."))
              (set-field! inventory actor (cons loot (get-field inventory actor)))
              (when (is-a? loot sapling-finger%) #;(win) (error "world.rkt: update-state!: Reimplement win!"))))
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
    ['parry
     (displayln "TODO implement this action")]
    ['brawl
     ; Resolve as attack action
     (define result (resolve-attack-action! world action))
     (displayln "brawl result:")
     (displayln result)
    ]
    
    [else (error (string-append "Unknown player action: " (symbol->string (action-symbol action))))]))

(define (resolve-enemy-action! world action)
  (case (action-symbol action)
    ['attack
     (define target (get-field pc world))
     (define target-defense 6) ; TODO: think! skills etc!
     (define attack-roll (d 2 6)) ; TODO think! what do the enemies actually attack as?
     (define damage (d 1 2))
     (define successful? (>= attack-roll target-defense))

     (displayln (string-append
                 "-- Enemy attack action: "
                 "Attack roll: 2d6 + skill = "
                 (number->string attack-roll)
                 " against PC defense: "
                 (number->string target-defense)
                 ))
     (when successful?
       (begin
         (displayln (string-append
                     "Success, damage: "
                     (number->string damage)
                     " HP"))
         (define attack-action-result (send target hit damage))
         (displayln attack-action-result)
         (displayln (string-append
                     "PC has "
                     (number->string (get-field hp target))
                     " HP"))))]
    [else (error (string-append "Unknown enemy action: " (symbol->string (action-symbol action))))]))

(define (resolve-action! world action)
  (define actor
    (if (eq? (action-actor action) 'pc)
        (get-field pc world)
        (action-actor action)))
  
  (if (is-a? actor pc%)
      (resolve-player-action! world action)
      (resolve-enemy-action! world action))
  (set-field!
   action-queue
   world
   (if (pair? (get-field action-queue world))
       (cdr (get-field action-queue world)) ; pop stack's topmost element
       '()))
  #;(displayln "ACTION RESOLVED")
  #;(displayln (get-field action-queue world)))

(define (add-action-to-queue world action)
  (define new-actions
    (append (get-field action-queue world)
            (list action)))
  (set-field! action-queue world new-actions))

(provide (all-defined-out))