#lang at-exp racket

(provide (all-defined-out))

(require
  "../../2-core/core.rkt"
  "../../2-core/io.rkt"
  "../../3-types/action.rkt"
  "../../3-types/actor.rkt"
  )

(require racket/lazy-require)

(lazy-require ["../../4-systems/world/world.rkt" (get-actor)])

; This is not serialized!
(define action-queue '())

(define (add-to-action-queue action initiative)
  (set! action-queue (cons (cons initiative action) action-queue)))

(define (remove-from-action-queue actions)
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

        (format "~a" (actor-name (get-actor (action-actor-id action)))))
      (tr action-description (format "~a" initiative))))

  (info-card actions "Action initiatives")
  (wait-for-confirm)

  (set! action-queue '())
  (for ([action-with-initiative sorted])
    (set! action-queue (append-element action-queue (cdr action-with-initiative))))

  action-queue)