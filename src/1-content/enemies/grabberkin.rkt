#lang at-exp racket

(provide (all-defined-out))

(require
  "../../0-engine/2-core/io.rkt"
  "../../0-engine/2-core/core.rkt"

  "../../0-engine/3-types/action.rkt"
  "../../0-engine/3-types/actor.rkt"
  "../../0-engine/3-types/status.rkt"

  "../../0-engine/4-systems/actors/actor.rkt"
  "../../0-engine/4-systems/checks/checks.rkt"

  "../../0-engine/3-types/stance.rkt"

  "../../0-engine/7-state/state.rkt"
  )

(require racket/lazy-require)

(lazy-require ["../../0-engine/6-combat/combat.rkt"
  (get-combatant-name
   display-combatant-info
   display-pc-combatant-info
   add-combat-flag
   )])


; Grabberkin are greedy -> they have jewelry -> they drop loot

;;; THE BIG THINGS:
;;; What is Grabberkin's purpose, gameplay-wise? What is its niche?
;;; What is its vulnerability?
;;; How much leeway there is? Should it be considered a mini-boss?
;;; Idea: Easy to win once you know how, but requires something specific that can be gotten easily if you know how
;;; -> act as a soft gate
;;; -> somewhat miniboss-like *at first*
; They are greedy. They are clingy. (What do they most wish?)
; Add 'steal' action, but make it pretty rare / once-per-encounter type thing maybe? (steal gold, not items)
; Idea: Vulnerabilities:
; - Firearms: Simply too much firepower. (Approach weaknesses: Makes noise, bullets are hard to come by, fuck up and blow your own leg up -> skill check if not proficient / consume LP)
; - Chainsaw: Too awesome. Weaknesses: Got to get up-and-close, makes noise, needs gas
; - Slashing damage, IF you know how to do it - because it cuts through tendons -> can't grab no more
; - Good perception (-> avoid)

; -> when selecting loadout, possibilities are bolt cutters or a chainsaw,
; gun has to be sourced by doing a favor to The Merchant, robbing a Cache, or finding your own corpse that had a gun in a prior run.
; guns: hunting rifles and shotguns in the wild, at the Facility also handguns and assault rifles.

; This doesn't belong here really but what the hell:
; - After combat, patching up is possible, but it is risk-free only if you know medicine, which requires finding literature (= doing a Cache-run, in practice) and then leveling up


(define (make-grabberkin)
  (define enemy (make-actor "Grabberkin" 'grabberkin #:max-hp 9 #:size 'large))

  (set-actor-dexterity! enemy 4)
  (set-actor-strength! enemy 11)
  (set-trait! enemy "defense" -1)
  (set-trait! enemy "melee-attack-skill" 1)
  (set-trait! enemy "hp-hidden" #f)
  enemy)

(define (make-grabberkin-action actor action-flag)
  (case action-flag
    ['pull-under
     (define target-id (actor-id (pc)))
     (make-action
      #:symbol 'pull-under
      #:actor actor
      #:duration 1
      #:target target-id
      #:tags '(initiative-based-resolution)
      #:details '()
      #:resolution-rules
      `(

        (p "The hands grasping her ankle – the thing with the hands – shift in the waters under the floating raft of moss. The thing pulls Otava through the moss, through a thick layer of algae, into the cloudy waters. The heavy, dark water closes in around her.")
        (p "The thing pulls her deeper. She fights back, but her arms get caught in the massive algae congesting the grimy waters. She cannot hold her breath much longer.") ; -> fragments -> saving throw, not direct death
        (wait-for-confirm)

        (p "Otava opens her mouth and drowns four feet under the surface of a nameless pool in Martaanvuo.")
        (add-feature-to-location! (current-location) 'drowning-pools)
        (kill (pc) 'drowned)


        )

      )]

    ['anklebreaker
     (define target-id (actor-id (pc)))
     (make-action
      #:symbol 'anklebreaker
      #:actor actor
      #:duration 1
      #:target target-id
      #:tags '(initiative-based-resolution)
      #:details '()
      #:resolution-rules
      `(

        (define target (pc))
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
                   (current-elapsed-time)
                   (FreshInjury 'ankle-broken "movement 3x slower"
                     (current-elapsed-time)
                     ))
                  (when critical?
                    (inflict-condition!
                     target
                     (current-elapsed-time)

                     ; TODO: content
                     (condition 'bleeding
                                "Bleeding, should get this patched ASAP."
                                )))
                  (display-combatant-info target)
                  'ok)
                 ('dead
                  (display-combatant-info target)
                  'pc-dead)
                 (else (error (format "unhandled action-result ~a" action-result))))
               )

              ; second ankle
              (else
               (p "The Grabberkin shifts its hands onto Otava's other ankle with ease, as if it's slowly waking up, and crushes the bones in Otava's other ankle, too.")

               (define critical? (roll-crit? 4))
               (when critical?
                 (p "A sharp edge of a broken bone punctures an artery and blood gushes out."))

               (define action-result (take-damage target 1 'trauma))
               (display-combatant-info target)
               (case action-result
                 ('hit
                  (inflict-condition!
                   target (condition
                           'ankle-broken
                           (current-elapsed-time)
                           "Can't walk, too painful."))
                  (when critical?
                    (inflict-condition!
                     target

                     (condition 'bleeding
                                (current-elapsed-time)
                                "Bleeding, should get this patched ASAP."
                                )))
                  'ok)
                 ('dead
                  'pc-dead)
                 (else (error (format "resolve-anklebreaker-action!: unhandled action-result ~a" action-result))))))
        ))]

    ['skip
     (make-action
      #:symbol 'skip
      #:actor actor
      #:duration 0
      #:target '()
      #:tags '(initiative-based-resolution)
      #:details '(slow silent))]

    ['grab
     (define strength (+ (d 1 3) 0))
     (define target-id (actor-id (pc)))
     (make-action
      #:symbol 'modify-status
      #:actor actor
      #:duration 0
      #:target target-id
      #:tags '(initiative-based-resolution slow)
      #:details (list (status 'bound strength))
      #:resolution-rules
      `(
        (define target (pc))
        (p "The Grabberkin seems to realize its grip is loosening. Its rotting fingers curl around Otava's ankle again with dreadful might.")
        (modify-actor-status-lifetime target 'bound ,strength)
        'ok
        )
      )]

    ['release-grip
     (make-action
      #:symbol 'release-grip
      #:actor actor
      #:duration 0
      #:target '()
      #:tags '(initiative-based-resolution)
      #:details '(fast)
      #:resolution-rules
      `(
        (p "The Grabberkin's hands let go of Otava's ankles and disappear under the moss.")
        (award-xp! 3 "for surviving an encounter with a Grabberkin")
        (remove-enemy ',(actor-id actor))
        ))]

    [else
     (error (format "make-grabberkin-action: unknown action ~a" action-flag))]))

(define (grabberkin-hp-above-threshold? actor)
  (define hp (actor-hp actor))
  (define max-hp (actor-max-hp actor))
  (define threshold (quotient max-hp 2))

  (> hp threshold))

(define (get-grabberkin-action-phase-1 actor)
  (cond
    ((and (actor-has-status-of-type? (pc) 'bound)
          (> (actor-lifetime-of-status-of-type? (pc) 'bound)
             1))
     (define options
       '(anklebreaker anklebreaker anklebreaker anklebreaker grab skip))

     (define roll (d 1 6))
     (define index (- roll 1))
     (define action (list-ref options index))

     (make-grabberkin-action actor action))
    (else
     (make-grabberkin-action actor 'grab))))

(define (get-grabberkin-action-phase-2 actor)
  (cond
    ((and (actor-has-status-of-type? (pc) 'bound)
          (> (actor-lifetime-of-status-of-type? (pc) 'bound)
             2))
     (define options
       '(pull-under pull-under pull-under grab skip skip))

     (define roll (d 1 6))
     (define index (- roll 1))
     (define action (list-ref options index))

     (make-grabberkin-action actor action))
    (else
     (make-grabberkin-action actor 'grab))))

(define (current-phase)
  (cond
    ((and (not (actor-has-condition-of-type? (pc) 'ankle-broken))
          (not (actor-has-condition-of-type? (pc) 'both-ankles-broken)))
     1)
    ((and (actor-has-condition-of-type? (pc) 'ankle-broken)
          (not (actor-has-condition-of-type? (pc) 'both-ankles-broken)))
     1)
    ((and (not (actor-has-condition-of-type? (pc) 'ankle-broken))
          (actor-has-condition-of-type? (pc) 'both-ankles-broken))
     2))
  )

(define (get-grabberkin-action actor)
  (cond ((and (in-combat?)
              (actor-in-range? actor 'close))
         (cond
           ((grabberkin-hp-above-threshold? actor)
            (case (current-phase)
              [(1) (get-grabberkin-action-phase-1 actor)]
              [(2) (get-grabberkin-action-phase-2 actor)]))

           (else
            (make-grabberkin-action actor 'release-grip))))
        (else
         '() ; not in combat, do nothing
         )))

; implicitly, this is the pre-own-action reaction
(define (get-grabberkin-reaction actor)
  (cond ((not (grabberkin-hp-above-threshold? actor))
         (make-grabberkin-action actor 'release-grip))
        (else
         '())))
