#lang at-exp racket


(provide (all-defined-out))

(require racket/lazy-require)

(require
  "../2-core/io.rkt"
  "../2-core/core.rkt"

  "../3-types/actor.rkt"
  "../3-types/location.rkt"
  "../3-types/pc-actor.rkt"
  "../3-types/task.rkt"

  "../4-systems/actors/actor.rkt"
  ;;; "../4-systems/pc/character-sheet.rkt"
  "../4-systems/world/world.rkt"

  "../6-combat/stance.rkt"
  )

(lazy-require ["state.rkt" (
  current-flags
  current-completed-fragments
  current-fragment-id
  current-in-combat?
  current-life
  current-pc
  current-tasks
  current-run
  current-once-per-day-actions-done
  )])

(lazy-require ["../6-combat/combat.rkt" (
  end-combat!
  )])

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

(define (once-per-day-action-done? action-symbol)
  (if (member action-symbol (current-once-per-day-actions-done))
      #t
      #f))

(define (mark-once-per-day-action-done! action-symbol)
  (when (not (once-per-day-action-done? action-symbol))
    (current-once-per-day-actions-done
      (append-element (current-once-per-day-actions-done)
                      action-symbol))))


; this could be a macro so that raw syntax "pc" in isolation would turn into "(pc)"
(define (pc)
  (current-pc))

; "clean up the board"
(define (remove-all-enemies-and-end-combat!)
  (for ([enemy (get-current-enemies)])
    (remove-actor-from-location! (get-location-by-id (actor-location-id enemy)) enemy))
  (set-actor-statuses! (pc) '())
  (end-combat!))


; actor or ActorId
(define (remove-enemy enemy)
  (when (or (symbol? enemy) (number? enemy)) (set! enemy (get-actor enemy)))
  (remove-actor-from-location! (get-location-by-id (actor-location-id enemy)) enemy))

(provide actor-in-range?)
(define (actor-in-range? enemy range)
  (define stance (actor-stance enemy))
  (eq? (stance-range stance) range))


(define (increment-achievement! achievement)
  (case achievement
    ['forgetful (displayln "forgetful achievement incremented")]
    [else
     (displayln "increment-achievement!: unknown achievement:")
     (displayln achievement)]))

(define (clear-current-fragment!)
  (current-fragment-id '()))

(define (fragment-completed? id)
  (memq id (current-completed-fragments))
)
