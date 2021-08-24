#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require rebellion/collection/association-list)

(require "action.rkt")
(require "actor.rkt")
(require "checks.rkt")
(require "condition.rkt")
(require "io.rkt")
(require "item.rkt")
(require "pc.rkt")
(require "state/state.rkt")
(require "stance.rkt")
(require "status.rkt")
(require "utils.rkt")
(require "world.rkt")

(lazy-require
 ["state/combat.rkt"
  (get-combatant-name
   display-combatant-info
   display-pc-combatant-info
   add-combat-flag
   )])

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
  #;(define success? (skill-check title skill action-target-number))
  (define success? #t)

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
  (when success? (set! action-result (take-damage target damage-roll-result 'melee)))
  (when (eq? action-result 'dead)
    
    ; TODO what's a smart place to store this? the actor?
    (case (actor-name (action-target action))
      [("Blindscraper") (award-xp! 7)]))

  (display-combatant-info target)
  (newline)

  ; Urgh, refactor!
  (when (eq? action-result 'dead)
    (if (not (pc-actor? (action-target action)))
        (p "The " (actor-name (action-target action)) " is dead.")
        (begin
          (p "Otava is dead.")
          (set! action-result 'pc-dead))))

  action-result
  )

(define (weapon-info gun)
  (define body
    (append-element
     (for/list ([item-detail (item-details gun)])
       (list (string-append " "
                            (car item-detail)
                            " ")
             (string-append " "
                            (~s (cdr item-detail))
                            " ")))
     (list (string-append " Ammo left ")
           (string-append " "
                          (number->string (ranged-weapon-ammo-left gun))
                          " "))))
  (info-card body (item-name gun)))

; helper that belongs to actor (or one layer above actor)
(define (get-firearm actor)
  (define items (actor-inventory actor))
  (findf (λ (item) (ranged-weapon? item))
         items)
  )

(define (resolve-successful-shoot-action! action)
  (define actor (action-actor action))
  (define target (action-target action))
  (define title
    (string-append "Ranged [firearms], "
                   (get-combatant-name actor)
                   " vs "
                   (get-combatant-name target)))

  ; TODO add sophistication regarding ranges etc
  (define success? #t)
  (consume-ammo! 1)

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
  (when success? (set! action-result (take-damage target damage-roll-result 'melee)))
  (when (eq? action-result 'dead)
    
    ; TODO what's a smart place to store this? the actor?
    (case (actor-name (action-target action))
      [("Blindscraper") (award-xp! 7)]))

  (display-combatant-info target)
  (newline)

  ; Urgh, refactor!
  (when (eq? action-result 'dead)
    (if (not (pc-actor? (action-target action)))
        #;(p "The " (actor-name (action-target action)) " is dead.")
        '()
        (begin
          #;(p "Otava is dead.")
          (set! action-result 'pc-dead))))

  action-result
  )

(define (resolve-shoot-action! action)
  (define actor (action-actor action))
  (define target (action-target action))
  (define gun (get-firearm actor))
  (case (ranged-weapon-ammo-left gun)
    [(0) (p "Click. Out of ammo.")
         (award-xp! 1 "Whoops.")
         (add-combat-flag 'aware-of-being-out-of-ammo)
         (increment-achievement! 'forgetful)
         'failure]
    [else (resolve-successful-shoot-action! action)]
    )
  
  
  )



; ability-like attack
(define (resolve-pull-under-action! action)
  (p "The hands grasping her ankle – the thing with the hands – shift in the waters under the floating raft of moss. The thing pulls Otava through the moss, through a thick layer of algae, into the cloudy waters. The heavy, dark water closes in around her.")
  (p "The thing pulls her deeper. She fights back, but her arms get caught in the massive algae congesting the grimy waters. She cannot hold her breath much longer.") ; -> fragments -> saving throw, not direct death
  (wait-for-confirm)

  (p "Otava opens her mouth and drowns four feet under the surface of a nameless pool in Martaanvuo.") ; -> todo: name it 'the drowning pools' in subsequent rounds
  (kill (pc) 'drowned)
  )

;;; CRIT ROLL IDEA:
; Base chances are like 1/6 for "certain" attack failing, 1/6 for crit,
; but PC's attribute/skill bonuses can shift that balance
; - or would failures be based on saving throw? maybe.

; move to utils
(define (roll-crit? sides)
  (define crit-roll (d 1 sides))
  (define critical? (= crit-roll 6))
  (define crit-string (if critical?
                          ", crit"
                          ""))
  (displayln (string-append "[crit roll: 1d"
                            (number->string sides)
                            " = " (number->string crit-roll) crit-string "]")))

; ability-like attack
(define (resolve-anklebreaker-action! action)
  (define target (action-target action))
  (cond ((not (actor-has-condition-of-type? target 'ankle-broken)) ; first
         (p "The hands tighten their vice-like hold on Otava's ankle. There's a wet, crunchy sound as bones shatter and tear through the surrounding muscle.")
         (define critical? (roll-crit? 4))
         (when critical?
           (p "A shard of bone sticks out through a gash in her ankle. Blood starts to flow."))
         (define action-result (take-damage target 1 'trauma))
         (case action-result
           ('hit
            (inflict-condition!
             target
             (condition 'ankle-broken "resolve-anklebreaker-action!: details for 'ankle-broken todo"))
            (when critical?
              (inflict-condition!
               target

               (condition 'bleeding ; TODO: This kind of involved definition belongs to, say, conditions.rkt or something
                          ;"resolve-anklebreaker-action!: details for 'bleeding todo"
                          '() ; details
                          )))
            (display-combatant-info target)
            'ok)
           ('dead
            (display-combatant-info target)
            'pc-dead)
           (else (error (string-append "resolve-anklebreaker-action!: unhandled action-result " (symbol->string action-result)))))
         )

        ; second ankle
        (else
         (p "The Grabberkin shifts its hands onto Otava's other ankle with ease, as if it's slowly waking up, and crushes the bones in Otava's other ankle, too.")

         (define critical? (roll-crit? 4))
         (when critical?
           (p "A sharp edge of a broken bone punctures an artery and blood gushes out."))

         (define action-result (take-damage (action-target action) 1 'trauma))
         (display-combatant-info (action-target action))
         (case action-result
           ('hit
            (inflict-condition!
             (action-target action) (condition
                                     'ankle-broken
                                     "resolve-anklebreaker-action!: details todo"))
            (when critical?
              (inflict-condition!
               target

               (condition 'bleeding ; TODO: This kind of involved definition belongs to, say, conditions.rkt or something
                          ;"resolve-anklebreaker-action!: details for 'bleeding todo"
                          '() ; details
                          #;(λ ()
                            (define bleed-damage-roll (d 1 6)) ; could give bonus from constitution here? say, 1d6?
                            (cond ((= 1 bleed-damage-roll)
                                   (displayln "[Bleed check: 1d6 = 1: [1] => 1 dmg]")
                                   (take-damage target 1 'bleed)
                                   (display-combatant-info target)
                                   )
                                  (else
                                   (displayln (string-append "[Bleed check: 1d6 = 1: ["
                                                             (number->string bleed-damage-roll)
                                                             "]]")))))


                          )))
            'ok)
           ('dead
            'pc-dead)
           (else (error (string-append "resolve-anklebreaker-action!: unhandled action-result " (symbol->string action-result))))))))


; ability-like attack
(define (resolve-go-to-engaged-action! action)
  (define lp (pc-actor-lp (pc)))
  (define dex (actor-dexterity (action-actor action)))
  (define success?
    (cond ((positive? lp)
           (displayln "[LP positive]")
           (attribute-check "Dexterity" dex))
          (else #t)))
           
  (if success?
      (begin
        (p "The Blindscraper suddenly leaps forward and gets a hold of Otava's forearm with a couple of its lanky fingers. One of its long claws is swinging free, looking for an opening.")
                 
        (let ([enemy-stance (stance "α" 'engaged "right")])
          (set-actor-stance! (action-actor action) enemy-stance)))
        
      (begin
        (p "The Blindscraper leaps at Otava, but she dives under it and stumbles back to her feet.")
        (displayln "[-1 LP]")
        (set-pc-actor-lp! (pc)
                          (- (pc-actor-lp (pc))
                             1))
        (when (< (pc-actor-lp (pc)) 0)
          (set-pc-actor-lp! (pc)
                            0))
        (displayln (pc-actor-lp (pc)))
        'failure))
  'ok
  )

(define (resolve-go-to-close-action! action)
  (define lp (pc-actor-lp (pc)))
  (define dex (actor-dexterity (action-actor action)))
           
  (p "The Blindscraper skitters towards Otava.")
  
  (let ([enemy-stance (stance "α" 'close "right")])
          (set-actor-stance! (action-actor action) enemy-stance))
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
           (p "After some time, Otava finds some edible fruits and roots. (" (number->string amount) " meals.)")
           (define item (list 'food (list amount)))
           (add-item-to-inventory! (pc) item)
           )
          (else
           (begin
             (p "Despite spending a while, Otava can't find anything to eat.")
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

(define (resolve-break-free-action! action)
  (define actor (action-actor action))
  (define details (action-details action))

  (define str-mod (vector-ref (association-list-ref details 'str-mod) 0))

  (define target (action-target action))
  (define target-stance (actor-stance target))

  
  (define statuses (actor-statuses actor))
  (define actor-bound-status
    (findf (λ (status) (eq? (status-type status) 'bound))
           statuses))

  (define target-number (status-lifetime actor-bound-status))

  (define dice-sides 10)
  (define bonus str-mod)
  (define roll (d 1 dice-sides))
  (define result (+ roll bonus))

  (define success?
    (cond ((= roll 1) #f)
          ((= roll dice-sides) #t)
          (else (> result target-number))))

  (define success-string
    (if success?
        "success"
        "failure"))

  (displayln
   (string-append "["
                  "Resolution: "
                  "1d10 + bonus > TN: "
                  (number->string roll)
                  " + "
                  (number->string bonus)
                  " = "
                  (number->string result)
                  " > "
                  (number->string target-number)
                  " - "
                  success-string
                  "]"))
  ; crit = nat MAX = always succeed,
  ; crit fail = nat 1 = always fail, avoid hard failures?
  (wait-for-confirm)
  (if success?
      (begin
        (displayln "Otava pulls her ankle free and stumbles back, just far enough to be out of reach of the writhing, searching hands.")
        (award-xp! 4)
        'end-combat)
      (begin
        (displayln "The grip is still too strong for Otava to break it.")
        (award-xp! 1)
        'failed)))


; skinnable, but in a sense generic action
(define (resolve-flee-action! action)
  (cond ((pc-actor? (action-actor action))
         (p "Otava turns her back to run.")
         (define skill (get-trait (pc) "athletics-skill"))

         (define stance-range-values '())
         (for ([enemy (get-current-enemies)])
           (define stance (actor-stance enemy))
           (define value (get-stance-range-numeric-value (stance-range stance)))
           (set! stance-range-values (append-element stance-range-values value)))
         (define target-number
           ; if there's an enemy in engaged range, then more difficult check
           (if (member 0 stance-range-values)
               10
               8))
           
         (define success? (skill-check "Athletics" skill target-number))
         (if success?
             (begin ; TODO wouldn't it be cool if only failure was explicitly noted :D
               (p "She dives behind a small bush and waits.")
               (wait-for-confirm)
               (if (luck-check)
                   (p "PASS")
                   (p "FAIL"))
               (p "Nothing seems to be following her.")
               (award-xp! 3 "for a working survival instinct")
               'end-combat)
             (begin
               (p "Otava's foot gets caught on a root. She falls face down in the mud.")
               (actor-add-status! (pc) (status 'fallen 1))
               (display-pc-combatant-info (pc))
               (wait-for-confirm)
               'failure))
         )

        (else ; not a pc actor
         (p
          (string-append
           (get-combatant-name (action-actor action))
           " tries to run."))
         (define skill 1)
         (define stance (actor-stance (action-actor action)))
         (define value (get-stance-range-numeric-value (stance-range stance)))
         (define target-number
           (if (= value 0)
               10
               8))

         (define success? (skill-check "Athletics" skill target-number))
         (if success?
             ; TODO this fails if there are multiple enemies!
             (begin
               (p "The Blindscraper skitters away and disappears in the foliage.")
               (award-xp! 1)
               'escape-from-combat)
             (begin
               (p "It is fast, but not fast enough.")
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
          ((eq? (action-symbol action) 'shoot)
           (resolve-shoot-action! action))

          ((eq? (action-symbol action) 'forage)
           (resolve-forage-action! action))
          ((eq? (action-symbol action) 'back-off)
           'ok
           )
          ((eq? (action-symbol action) 'go-to-location)
           'ok
           )
          ((eq? (action-symbol action) 'traverse)
           'ok
           )
          ((eq? (action-symbol action) 'cancel-traverse)
           'ok
           )
          ((eq? (action-symbol action) 'search-for-paths)
           (define exploration-skill (get-trait (pc) "exploration-skill"))
           (define target-number 9)
           (define exploration-check-result (skill-check "Exploration" exploration-skill target-number))
           #;(when exploration-check-result (expose-neighbor! (current-location)))
           (displayln "TODO: Redo exploration things")
           (if exploration-check-result
               'ok
               'failure
               ))
          ((eq? (action-symbol action) 'sleep)
           (p "Otava turns in for the night. Get some rest.")
           'ok)
          ((eq? (action-symbol action) 'flee)
           (resolve-flee-action! action)
           )

          ((eq? (action-symbol action) 'break-free)
           (resolve-break-free-action! action))

          ((eq? (action-symbol action) 'go-to-engaged)
           (resolve-go-to-engaged-action! action))
          ((eq? (action-symbol action) 'go-to-close)
           (resolve-go-to-close-action! action))

          ((eq? (action-symbol action) 'inflict-status)
           (define target (action-target action))
           (define status (car (action-details action)))
           (when (status? status)
             (when (eq? (status-type status) 'bound)
             (p "The Grabberkin seems to realize its grip is loosening. Its rotting fingers curl around Otava's ankle again with dreadful might.")))
           
           (inflict-status! target status)
           'ok
           )

          ((eq? (action-symbol action) 'modify-status)
           (define target (action-target action))
           (define status (car (action-details action)))
           (when (eq? (status-type status) 'bound) ; this is shit, refactor
             (p "The Grabberkin seems to realize its grip is loosening. Its rotting fingers curl around Otava's ankle again with dreadful might.")
             (define amount (status-lifetime status))
             (modify-actor-status-lifetime target 'bound amount)
             )
           'ok
           )

          ((eq? (action-symbol action) 'inflict-condition)
           (define target (action-target action))
           (define condition (car (action-details action)))
           (displayln "action-resolver: resolve-action!: inflict-condition: TODO")
           #;(when (eq? (status-type status) 'bound)
               (p "The Grabberkin seems to realize its grip is loosening. Its rotting fingers curl around Otava's ankle again with dreadful might."))
           #;(inflict-status! target status)
           'ok
           )

          

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