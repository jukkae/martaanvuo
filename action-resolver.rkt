#lang racket

(provide (all-defined-out))

(require racket/lazy-require)


(require "action.rkt")
(require "actor.rkt")
(require "checks.rkt")
(require "io.rkt")
(require "situation.rkt")
(require "stance.rkt")
(require "status.rkt")
(require "utils.rkt")
(require "world.rkt")

; "generic" attack with type
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

  ; Urgh, refactor!
  (when (eq? action-result 'dead)
    (if (not (pc-actor? (action-target action)))
        (paragraph "The " (actor-name (action-target action)) " is dead.")
        (begin
          (paragraph "Otava is dead.")
          (set! action-result 'pc-dead))))

  action-result
  )

; "generic" attack with type and flavor
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

  ; Urgh, refactor!
  (when (eq? action-result 'dead)
    (if (not (pc-actor? (action-target action)))
        (paragraph "The " (actor-name (action-target action)) " is dead.")
        (begin
          (paragraph "Otava is dead.")
          (set! action-result 'pc-dead))))

  action-result
  )

; ability-like attack
(define (resolve-pull-under-action! action)
  (define damage-roll (λ () (d 1 4)))
  #;(define details
    (list
     (list "öö" "ää")
     ))
  #;(info-card details "The hand pulls harder and Otava is sucked under the wet soil.")
  (paragraph "The thing grasping her ankle splashes in the waters under the moss, as it searches for better hold. It finds something and pulls with fervor. Otava tumbles on the soggy moss as the thing pulls her under the moss, through a thick layer of algae, and into the cloudy waters. This breath might be her last, so she fills he lungs with air. The wet heavy darkness closes in around her.")
  (paragraph "She fights back, but her hands get caught in various dismal kinds of aquatic flora and her strikes fail to land. She is getting tired and cannot hold much longer.")
  (wait-for-confirm)
  ;(define damage-roll (λ () (d 1 4)))
  (paragraph "Otava opens her mouth and drowns four feet under the surface of a nameless quagmire near Martaanvuo.")
  (kill (pc) 'drowned)
  )

; ability-like attack?
(define (resolve-anklebreaker-action! action)
  (define roll "2d6+whatever")
  (displayln "Anklebreaker resolution")
  (displayln roll)
  'ok
  )

; ability-like attack?
(define (resolve-tighten-grip-action! action)
  (define roll "2d6+whatever")
  (displayln "The gk tightens its grip")
  (displayln roll)
  'ok
  )

; ability-like attack
(define (resolve-go-to-engaged-action! action)
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
        (hash-remove! (situation-enemy-stances *situation*) (action-actor action))
                 
        (let ([enemy-stance (stance "α" 'engaged "right")])
          (hash-set! (situation-enemy-stances *situation*) (action-actor action) enemy-stance)))
        
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

; just a skill check in a fancy coat
(define (resolve-forage-action! action)

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
    (if successful?
        'successful
        'failure)))


; skinnable, but in a sense generic action
(define (resolve-flee-action! action)
  (cond ((pc-actor? (action-actor action))
         (paragraph "Otava turns her back to run.")
         (define skill (get-trait (situation-pc *situation*) "athletics-skill"))

         (define stance-range-values '())
         (for ([(k v) (in-hash (situation-enemy-stances *situation*))])
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
               (actor-add-status! (action-target action) (status 'fallen 1))
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
         (define stance (hash-ref (situation-enemy-stances *situation*) (action-actor action)))
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
               (actor-add-status! (action-actor action) (status 'fallen 1))
               (display-combatant-info (action-actor action))
               'failure))
         )))



;; This should probably be formalized and eventually provided as a contract or something
; can return:
; 'pc-dead  when the pc is dead as a consequence of this action
; 'ok       when the action is completely resolved and not explicitly successful or failed
; 'success  when the action is completely resolved and explicitly successful
; 'failed   when the action is completely resolved and fails
; this should just dispatch, doing something else smells bad
(define (resolve-action! action)
  (when (actor-alive? (action-actor action))
    (cond ((eq? (action-symbol action) 'melee)
           (resolve-melee-action! action))

          ((eq? (action-symbol action) 'wrestle)
           (resolve-wrestle-action! action))

          ((eq? (action-symbol action) 'forage)
           (resolve-forage-action! action))
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
           (when exploration-check-result (expose-neighbor! (current-location)))
           (if exploration-check-result
               'ok
               'failure
               ))
          ((eq? (action-symbol action) 'sleep)
           (paragraph "Otava turns in for the night. Get some rest.")
           'ok)
          ((eq? (action-symbol action) 'flee)
           (resolve-flee-action! action)
           )

          ; This is starting to get unwieldy... but get poc done first
          ((eq? (action-symbol action) 'go-to-engaged)
           (resolve-go-to-engaged-action! action))

          ((eq? (action-symbol action) 'inflict-status)
           (define target (action-target action))
           (define status (car (action-details action)))
           (inflict-status! target status)
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