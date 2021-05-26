#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(lazy-require
 ["martaanvuo.rkt"
  (award-xp!
   handle-exploration-check-result!
   wait-for-confirm
   )])

(require "action.rkt")
(require "actor.rkt")
(require "checks.rkt")
(require "io.rkt")
(require "situation.rkt")
(require "utils.rkt")

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


;; This should probably be formalized and eventually provided as a contract or something
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
                        (display-pc-combatant-info (pc))
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