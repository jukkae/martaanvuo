#lang at-exp racket

(require rebellion/collection/association-list)

(require
  "combat.rkt"

  "../actions/action.rkt"
  "../actions/choice.rkt"

  "../actors/actor.rkt"

  "../combat/stance.rkt"

  "../core/utils.rkt"

  "../state/state.rkt")

(provide get-combat-choices)
(define (get-combat-choices)
  (define targets (get-current-enemies))

  (define combat-choices '())

  (when (actor-has-item? (pc) 'bolt-cutters)
    (set! combat-choices
          (append combat-choices (get-melee-choices))))

  (when (actor-has-item? (pc) 'revolver)
    (set! combat-choices
          (append combat-choices (get-ranged-choices))))

  (cond ((and (not (engaged?))
              (not (actor-has-status-of-type? (pc) 'bound)))
         (define run-choice
           (make-choice
            'flee
            "Run"
            (λ ()
              (make-action
               #:symbol 'flee
               #:actor 'pc
               #:duration 1
               #:tags '(initiative-based-resolution fast)
               #:resolution-rules
               `((resolve-flee-action! 'pc))))))
         (set! combat-choices (append-element combat-choices run-choice))))

  (define close-enemies (get-enemies-at-range 'close))
  (define close-grabberkin
    (filter (λ (enemy) (equal? (actor-type enemy)
                               'grabberkin))
            close-enemies))

  (cond ((not (null? close-grabberkin))

         (define strength-mod (get-attribute-modifier-for (actor-strength (pc))))

         (define details
           (association-list 'str-mod strength-mod))

         (define target-id
           (actor-id (take-random close-grabberkin)))

         (define break-free-choice
           (make-choice
            'pull-free
            "Pull the leg free."
            (λ ()
              (make-action
               #:symbol 'break-free
               #:actor (pc)
               #:duration 1
               #:target target-id
               #:tags '(initiative-based-resolution fast)
               #:details details
               #:resolution-rules
               `((resolve-break-free-action! (pc) ,target-id ,strength-mod))))))
         (set! combat-choices (append-element combat-choices break-free-choice))))

  combat-choices
  )


(define (get-melee-choices)
  (define targets (get-current-enemies))
  (define combat-choices '())
  (for ([i (in-range 0 (length targets))])
    (define target (list-ref targets i))
    (define stance (actor-stance target))
    (cond ((or (eq? (stance-range stance) 'close)
               (eq? (stance-range stance) 'engaged))
           ;(define damage-roll '(λ () d 1 2))
           (define details
             `(list
               (cons 'damage-roll ,'(λ () (d 1 2)))
               (cons 'damage-roll-formula "1d2")
               (cons 'damage-type 'bludgeoning)
               ))
           (define choice
             (make-choice
              'attack
              (format "Hit ~a [with bolt cutters]." (get-combatant-name target))
              (λ ()
                (define target-id (actor-id target))
                (make-melee-attack-action
                 #:actor (pc)
                 #:duration 1
                 #:target target-id
                 #:n 1
                 #:x 2
                 #:bonus 0
                 ))))
           (set! combat-choices (append-element combat-choices choice)))
          ))
  combat-choices)

; implementation detail
(define (get-ranged-choices)
  (define targets (get-current-enemies))

  (define all-choices
    (for/list ([i (in-range 0 (length targets))])
      (define target (list-ref targets i))
      (define target-id (actor-id target))
      (define stance (actor-stance target))

      (list
       (when (and (not (flag-set? 'aware-of-being-out-of-ammo))
                  (or (eq? (stance-range stance) 'far) ; always require roll
                      (eq? (stance-range stance) 'mid) ; require roll if no proficiency
                      (eq? (stance-range stance) 'close) ; never require roll
                      (eq? (stance-range stance) 'engaged)))
         (define damage-roll (λ () (d 2 2)))
         (define details
           (list
            (cons 'damage-roll damage-roll)
            (cons 'damage-roll-formula "2d2")
            (cons 'damage-type 'gunshot) ; we're assuming firearms here
            ))

         (make-choice
          'attack
          (format "Shoot ~a [with revolver]." (get-combatant-name target))
          (λ ()
            (make-action
             #:symbol 'shoot
             #:actor (pc)
             #:duration 1
             #:target target-id
             #:tags '(initiative-based-resolution)
             #:details details
             #:resolution-rules
             '(resolve-as-shoot-action)))))

       (when (or (eq? (stance-range stance) 'close)
                 (eq? (stance-range stance) 'engaged))

         (define details
           `(list
             (cons 'damage-roll '(λ () 1))
             (cons 'damage-roll-formula "1")
             (cons 'damage-type 'bludgeoning)
             ))
         (make-choice
          'attack
          (format "Pistol whip the ~a [with revolver]." (get-combatant-name target))
          (λ ()
            (define target-id (actor-id target))
            (make-melee-attack-action
             #:actor (pc)
             #:duration 1
             #:target target-id
             #:n 1
             #:x 2
             #:bonus 0
             ))))
       )))

  (condense all-choices))
