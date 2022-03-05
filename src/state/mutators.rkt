#lang at-exp racket


(provide (all-defined-out))

(require racket/lazy-require)

(require
  "../actors/actor.rkt"

  "../core/io.rkt"
  "../core/utils.rkt"

  "../locations/0-types/location.rkt"

  "../pc/character-sheet.rkt"

  "../tasks/task.rkt"

  "../world/world.rkt"

  "../combat/stance.rkt")

(lazy-require
 ["state.rkt" (current-flags
               current-fragment-id
               current-in-combat?
               current-life
               current-pc
               current-tasks
               current-run)])

(lazy-require
 ["../combat/combat.rkt" (end-combat!)])

;; bad name for this file, this is sort of "misc"

(define (set-flag flag)
  (when (not (flag-set? flag))
    (current-flags (append-element (current-flags) flag))))

(define (remove-flag flag)
  (when (flag-set? flag)
    (current-flags (remq flag (current-flags)))))

(define (flag-set? flag)
  (memq flag (current-flags)))

(define (print-flags)
  (dev-note "print-flags:")
  (displayln (current-flags)))


(define (in-combat?) (current-in-combat?))

(define (engaged?)
  (define any-enemy-engaged? #f)

  (for ([enemy (get-current-enemies)])
    (let ([stance (actor-stance enemy)])
      (when (eq? (stance-range stance) 'engaged)
        (set! any-enemy-engaged? #t))))

  any-enemy-engaged?)

(define (get-an-enemy-at-range range)
  (define current-enemies (get-current-enemies))
  (define enemies-shuffled (shuffle current-enemies))
  (define enemy-in-range '())
  (for ([enemy enemies-shuffled])
    (define stance (actor-stance enemy))
    (when (eq? (stance-range stance) range)
      (set! enemy-in-range enemy)))
  enemy-in-range)

(define (get-enemies-at-range range)
  (define current-enemies (get-current-enemies))
  (define enemies-in-range '())
  (for ([enemy current-enemies])
    (define stance (actor-stance enemy))
    (when (eq? (stance-range stance) range)
      (set! enemies-in-range (append-element enemies-in-range enemy))))
  enemies-in-range)

(define (includes-enemy-of-type enemies type)
  (findf (λ (enemy) (eq? (actor-type enemy) type))
         enemies))

(define (in-range? target attack-mode)
  (case attack-mode
    ['melee
     (dev-note "in-range? tbd")
     #t]
    [else
     (dev-note "in-range? tbd")
     #f]))

(define (current-location)
  (get-location-by-id (actor-location-id (pc))))

(define (get-current-enemies)
  (filter
   (λ (actor) (and (actor-alive? actor)
                   (not (pc-actor? actor))))
   (location-actors (current-location))))

(define (get-current-enemy)
 (define enemies (get-current-enemies))
 (cond ((null? enemies)
        #f)
       ((> 1 (length enemies))
        (dev-note "get-current-enemy: Multiple enemies present!")
        (car enemies))
       (else
        (car enemies)))
)

(define (tasks)
  (current-tasks))

(define (find-task id)
  (findf (λ (t) (eq? (task-id t) id))
         (current-tasks)))



(define (reduce-debt-by! amount)
  (dev-note "FIXME: debt amount modification")
  '())

; this could be a macro so that raw syntax "pc" in isolation would turn into "(pc)"
(define (pc)
  (current-pc))

(define (remove-all-enemies-and-end-combat!)
  (for ([enemy (get-current-enemies)])
    (remove-actor-from-location! (get-location-by-id (actor-location-id enemy)) enemy))
  (end-combat!)
  (dev-note "post-combat steps")) ; for instance, wound care (fast vs good), xp, summary etc


; actor or ActorId
(define (remove-enemy enemy)
  (when (or (symbol? enemy) (number? enemy)) (set! enemy (get-actor enemy)))
  (remove-actor-from-location! (get-location-by-id (actor-location-id enemy)) enemy))

(provide actor-in-range?)
(define (actor-in-range? enemy range)
  (define stance (actor-stance enemy))
  (eq? (stance-range stance) range))

(define (pick-up-items!)
  (p "Otava picks up everything there is to pick up.")
  (define all-items (location-items (current-location)))
  (for ([item all-items])
    (remove-item-from-location! (current-location) item)
    (add-item-to-inventory! (pc) item))
  (inventory))

(define (increment-achievement! achievement)
  (case achievement
    ['forgetful (displayln "forgetful achievement incremented")]
    [else
     (displayln "increment-achievement!: unknown achievement:")
     (displayln achievement)]))

(define (unset-current-fragment-id!)
  (current-fragment-id '()))
