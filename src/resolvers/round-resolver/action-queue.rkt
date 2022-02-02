#lang at-exp racket

(provide (all-defined-out))

(require
  "../../actors/actor.rkt"
  "../../actions/action.rkt"
  "../../core/io.rkt"
  "../../core/utils.rkt")

; This is not serialized!
(define action-queue '())

(define (add-to-action-queue action)
  (set! action-queue (cons action action-queue)))

(define (remove-from-action-queue actions)
  (set! action-queue (remq* actions action-queue)))

(define (clear-action-queue!)
  (set! action-queue '()))


(define (sort-action-queue)

  (define actions-by-initiatives '())
  (for ([action action-queue])
    (define actor (action-actor action))
    (define dexterity-mod (get-attribute-modifier-for (actor-dexterity actor)))

    (define action-mod 0)

    (cond ((has-tag? action 'fast)
           (set! action-mod 2))
          ((has-tag? action 'slow)
           (set! action-mod -4)))

    (define dice-1 (d 1 6))
    (define dice-2 (d 1 6))

    (define total (+ dice-1 dice-2 action-mod dexterity-mod))

    (set! actions-by-initiatives (append-element actions-by-initiatives (cons total action))))

  (define shuffled (shuffle actions-by-initiatives)) ; shuffle to avoid sort stability
  (define sorted (sort shuffled
                       (λ (a1 a2) (> (car a1) (car a2))))) ; intentionally flipped: Higher is better

  (define actions
    (for/list ([action-with-initiative sorted])
      (define action (cdr action-with-initiative))
      (define initiative (car action-with-initiative))
      (define action-description
        (format " ~a " (actor-name (action-actor action)))) ; action is hidden information
      (tr action-description (format " ~a " initiative))))
  ; TODO: Only show initiatives when more than one combatant do something, and exclude the ones that only "skip"
  (info-card actions "Action initiatives")
  (wait-for-confirm)

  (set! action-queue '())
  (for ([action-with-initiative sorted])
    (set! action-queue (append-element action-queue (cdr action-with-initiative))))

  action-queue)
