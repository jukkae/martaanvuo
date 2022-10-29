#lang at-exp racket

(require racket/lazy-require

         "ai.rkt"
         "action-queue.rkt"

         "../2-action-resolver/action-resolver.rkt"

         "../../2-core/io.rkt"
         "../../2-core/core.rkt"

         "../../3-types/action.rkt"
         "../../3-types/actor.rkt"
         "../../3-types/status.rkt"

         "../../4-systems/actors/actor.rkt"
         "../../4-systems/actors/statuses.rkt"
         "../../4-systems/checks/checks.rkt"
         "../../4-systems/pc/pc.rkt"
         "../../4-systems/world/world.rkt"

         "../../6-combat/combat.rkt"
         "../../6-combat/combat-action-resolver.rkt"

         "../../7-state/state.rkt")

(lazy-require ["../../../1-content/narration/combat-narration.rkt" (get-combatant-name)])

(provide resolve-turns!)
(define (resolve-turns!)
  (let/ec
   end-round-early
   (when (all-actions-of-type? action-queue 'escape)
     (award-xp! 1)
     (end-combat)
     (end-round-early))
   (for ([action action-queue])

     (define actor (get-actor (action-actor-id action)))
     (define actor-removed? (or (not actor) (empty? (actor-location-id actor))))

     (cond
       [(and (not actor-removed?) (not (equal? (action-symbol action) 'discarded)))
        (define turn-result 'ok)

        (cond
          [(or (equal? (action-symbol action) 'approach) (equal? (action-symbol action) 'retreat))

           (define opposing-actions
             (filter (λ (x) (not (eq? (action-symbol x) (action-symbol action))))
                     (cond
                       [(pc-action? action) (find-all-enemy-movement-actions)]
                       [else (find-pc-movement-actions)])))

           (when (not (empty? opposing-actions))
             (define all-mvmt-actions (append (list action) opposing-actions))

             ; (define all-movement-actions (append (find-pc-movement-actions) (find-all-enemy-movement-actions)))

             (define names
               (for/list ([mvmt-action all-mvmt-actions])
                 (get-combatant-name (get-actor (action-actor-id mvmt-action)))))
             (define longest (find-longest names))
             (define max-name-width (+ 2 (string-length longest))) ; []

             (notice "Contested movement roll")
             (define results '())
             (for ([a all-mvmt-actions])
               (define bonus (if (pc-action? a) 3 '()))
               (append-element!
                results
                (cons
                 a
                 (just-roll
                  "2d6"
                  #:bonus bonus
                  #:title (~a (format "[~a]" (get-combatant-name (get-actor (action-actor-id a))))
                              #:min-width max-name-width)
                  #:on-critical-success
                  (λ ()
                    (notice "Critical success, gain fast!")
                    (actor-add-status! (get-actor (action-actor-id a)) (status 'fast 1)))
                  #:on-critical-failure
                  (λ ()
                    (notice "Critical failure, lose fast and gain fallen!")
                    (actor-remove-status-of-type! (get-actor (action-actor-id a)) 'fast)
                    (actor-add-status! (get-actor (action-actor-id a)) (status 'fallen 1)))))))

             (define sorted (sort results (λ (x y) (> (cdr x) (cdr y)))))

             (cond
               [(equal? (action-actor-id (car (car sorted))) 'pc)
                (notice "Enemy movements discarded.")
                (discard-actions! (find-all-enemy-movement-actions))]
               [else
                (notice "Otava's movement discarded.")
                (discard-actions! (find-pc-movement-actions))]))])

        (define pre-action-reaction? (get-pre-action-reaction action))
        (when (not (null? pre-action-reaction?))
          (set! action pre-action-reaction?))

        (when (not (equal? (action-symbol action) 'discarded))
          (set! turn-result (resolve-turn! action)))

        (define post-action-reaction-from-target? (get-post-action-reaction action turn-result))
        (when (not (null? post-action-reaction-from-target?))
          ;(define action post-action-reaction-from-target?)
          (dev-note (format "-- post-action-reaction-from-target?: ~a"
                            post-action-reaction-from-target?)))

        (when (empty? (get-current-enemies))
          (set! turn-result 'end-combat))

        (when (not (pc-is-alive?))
          (set! turn-result 'pc-dead))

        (case turn-result
          ['pc-dead
           (end-combat)
           (end-round-early)]

          ['end-combat
           (end-combat)
           (end-round-early)])]

       [else 'skipped]))))

(define (ranged-attack-action? action)
  (if (equal? (action-symbol action) 'shoot)
    #t
    #f))

; (: -> Action TurnResult)
(define (resolve-turn! action)
  (let/ec return
          (define actor (get-actor (action-actor-id action)))
          (when (not actor)
            (return 'actor-removed))
          (when (not (null? (action-target action)))
            (define target (get-actor (action-target action)))
            (when (not target)
              (return 'target-removed)))
          (define turn-result
            (cond
              [(melee-attack-action? action) (resolve-melee-action! action)]
              [(equal? (action-symbol action) 'shoot)
               (resolve-shoot-action! action)]
              [else (resolve-action! action)]))
          (when (not (pc-is-alive?))
            return
            'pc-dead)
          turn-result))

(define (end-combat)
  (remove-all-enemies-and-end-combat!)
  (clear-action-queue!))
