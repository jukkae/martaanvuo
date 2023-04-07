#lang at-exp racket

(provide (all-defined-out))

(require "common.rkt"

         "../../0-engine/2-core/io.rkt"
         "../../0-engine/2-core/core.rkt"

         "../../0-engine/3-types/action.rkt"
         "../../0-engine/3-types/actor.rkt"

         "../../0-engine/4-systems/actors/actor.rkt"
         "../../0-engine/4-systems/pc/pc.rkt"
         "../../0-engine/4-systems/world/world.rkt"

         "../../0-engine/3-types/stance.rkt"

         "../../0-engine/7-state/state.rkt")

(define (make-voidfloater)
  (define enemy (make-actor "voidfloater" 'voidfloater #:max-hp 3 #:size 'small))
  (set-actor-dexterity! enemy 13)
  (set-trait! enemy "defense" 1)
  (set-trait! enemy "melee-attack-skill" 1)
  enemy)

(define (fight-behavior actor)
  (cond
    [(pc-envenomed-peaking?)
     (case (stance-range (actor-stance actor))
       [(engaged adjacent)
        (define target-id (actor-id (pc)))
        (make-melee-attack-action #:actor actor
                                  #:duration 1
                                  #:target target-id
                                  #:n 1
                                  #:x 2
                                  #:bonus 0)]
       [else (approach-action actor)])]
    [(actor-has-condition-of-type? (pc) 'envenomed) ; envenomed, but not yet peaking
     (case (stance-range (actor-stance actor))
       [(engaged adjacent close) (retreat-action actor)]
       [else (get-skip-action actor)])]
    [else
     (case (stance-range (actor-stance actor))
       ['engaged
        (define target-id (actor-id (pc)))
        (define subject-id (actor-id actor))
        (make-action
         #:symbol 'inject-venom
         #:actor actor
         #:duration 1
         #:target 'pc
         #:tags
         '(initiative-based-resolution) ; TODO: what to do with non-initiative-based actions in queue? just cancel?
         #:resolution-rules
         ; check *current* range, at time of resolving action
         `((case (stance-range (actor-stance (get-actor ,subject-id)))
             ['engaged
              (notice "The voidfloater's venom injector pierces Otava's skin.")
              (inflict-condition! (pc)
                                  (Illness 'envenomed
                                           (current-elapsed-time)
                                           (list 'at (current-elapsed-time) "ι")
                                           'incubating))]
             ['adjacent
              (cond
                [(= (d 1 2) 2)
                 (notice "[1d2: 2] The voidfloater's venom injector pierces Otava's skin.")
                 (inflict-condition! (pc)
                                     (condition 'envenomed
                                                (current-elapsed-time)
                                                (list 'at (current-elapsed-time) "ι")))]
                [else (notice "[1d2: 1] The voidfloater barely misses Otava.")])]
             [else (notice "The voidfloater is too far to reach Otava.")])
           '()))]
       [else (approach-action actor)])]))

(define (flee-behavior actor)
  (cond
    [(pc-envenomed-peaking?)
     (case (stance-range (actor-stance actor))
       [(engaged adjacent)
        (define target-id (actor-id (pc)))
        (make-melee-attack-action #:actor actor
                                  #:duration 1
                                  #:target target-id
                                  #:n 1
                                  #:x 2
                                  #:bonus 0)]
       [else (approach-action actor)])]
    [(actor-has-condition-of-type? (pc) 'envenomed)
     (cond
       [(or (equal? (stance-range (actor-stance actor)) 'far)
            (equal? (stance-range (actor-stance actor)) 'nearby))
        (notice "The voidfloater lurks, waiting.")
        (get-skip-action actor)]
       [else (retreat-action actor)])]
    [else
     (cond
       [(equal? (stance-range (actor-stance actor)) 'far)
        (define id (actor-id actor))

        (make-action #:symbol 'escape
                     #:actor actor
                     #:duration 1
                     #:target '()
                     #:tags '(initiative-based-resolution fast)
                     #:details '()
                     #:resolution-rules
                     `((notice (format "~a tries to escape." (get-combatant-name (get-actor ,id))))
                       (define skill 1)
                       (define stance (actor-stance (get-actor ,id)))
                       (define value (stance-range stance))
                       (define target-number (if (equal? value 'engaged) 10 8))
                       ; (define success? (skill-check "Athletics" skill target-number))
                       (define success? #t)
                       (if success?
                           (begin
                             (notice "The voidfloater escapes.")
                             (award-xp! 1)
                             (remove-actor-from-its-current-location! (get-actor ,(actor-id actor)))
                             'ok)
                           (begin
                             'failure))))]
       [else (retreat-action actor)])]))

(define (get-voidfloater-action actor)
  (cond
    [(> (actor-hp actor) (/ (actor-max-hp actor) 2)) (fight-behavior actor)]
    [else (flee-behavior actor)]))

(define (get-voidfloater-reaction actor)
  '())
