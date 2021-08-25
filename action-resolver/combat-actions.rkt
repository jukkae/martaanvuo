#lang racket

(provide
 (all-defined-out))

(require racket/lazy-require)

(require rebellion/collection/association-list)

(require "../action.rkt")
(require "../actor.rkt")
(require "../checks.rkt")
(require "../condition.rkt")
(require "../io.rkt")
(require "../item.rkt")
(require "../location.rkt")
(require "../pc.rkt")
(require "../route.rkt")
(require "../state/state.rkt")
(require "../state/logging.rkt")
(require "../stance.rkt")
(require "../status.rkt")
(require "../utils.rkt")
(require "../world.rkt")

(require "../round-resolver/event.rkt"
         "../round-resolver/simulation.rkt"
         "../round-resolver/timeline.rkt")


(lazy-require
 ["state/combat.rkt"
  (get-combatant-name
   display-combatant-info
   display-pc-combatant-info
   add-combat-flag
   )])

(lazy-require
 ["locations.rkt"
  (describe-begin-traverse-action
   describe-finish-traverse-action
   describe-cancel-traverse-action
   location-on-enter!
   )])

(lazy-require
 ["round-resolver/event-handler.rkt"
  (handle-interrupting-event!
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
         (set-flag 'aware-of-being-out-of-ammo)
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