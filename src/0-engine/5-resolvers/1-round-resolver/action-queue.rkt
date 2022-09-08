#lang at-exp racket

(provide (all-defined-out))

(require
  "../../2-core/core.rkt"
  "../../2-core/io.rkt"
  "../../3-types/action.rkt"
  "../../3-types/actor.rkt"
  "../../4-systems/actors/statuses.rkt"
  "../../4-systems/world/world.rkt"
  )

(define action-queue '()) ; not serialized, not meant to be

(define (add-to-action-queue action initiative)
  (set! action-queue (cons (cons initiative action) action-queue)))

(define (discard-action! action)
  (set-action-symbol! action 'discarded)
  (set! action-queue (remq action action-queue)))

(define (discard-actions! actions)
  (for ([action actions])
    (set-action-symbol! action 'discarded))
  (set! action-queue (remq* actions action-queue)))

(define (clear-action-queue!)
  (set! action-queue '()))

(define (sort-action-queue)

  (define shuffled (shuffle action-queue)) ; shuffle to avoid sort stability
  (define sorted (sort shuffled
                       (λ (a1 a2) (> (car a1) (car a2))))) ; intentionally flipped: Higher is better

  (define actions
    (for/list ([action-with-initiative sorted])
      (define action (cdr action-with-initiative))
      (define initiative (car action-with-initiative))
      (define action-description

        (format "~a~a"
          (actor-name (get-actor (action-actor-id action)))
          (if (actor-has-status-of-type? (get-actor (action-actor-id action)) 'fast)
            " [fast: +4]"
            "")))
      (tr action-description (format "~a" initiative))))

  (info-card actions "Action initiatives [higher is faster]")
  (wait-for-confirm)

  (set! action-queue '())
  (for ([action-with-initiative sorted])
    (set! action-queue (append-element action-queue (cdr action-with-initiative))))

  action-queue)

(define (movement-action? action)
  (if (or (eq? (action-symbol action) 'get-closer)
          (eq? (action-symbol action) 'get-further))
      #t
      #f))

(define (non-pc-action? action)
  (if (eq? (action-actor-id action) 'pc)
    #f
    #t))

(define (find-all-enemy-movement-actions)
  (filter non-pc-action? (filter movement-action? action-queue)))

(define (find-pc-movement-actions)
  (filter pc-action? (filter movement-action? action-queue)))
