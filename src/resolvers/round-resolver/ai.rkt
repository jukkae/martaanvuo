#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require
  "../../actions/action.rkt"

  "../../actors/actor.rkt"

  "../../core/utils.rkt"

  "../../enemies/blindscraper.rkt"
  "../../enemies/grabberkin.rkt"

  "../../state/state.rkt")

(require "action-queue.rkt")

(lazy-require
 ["round-resolver.rkt" (get-next-pc-action)]
 ["../../world/world.rkt" (get-actor)])


(define (get-next-npc-action actor)
  (case (actor-name actor)
    (["Blindscraper"] (get-blindscraper-action actor))
    (["Grabberkin"] (get-grabberkin-action actor))
    (else (displayln "get-next-npc-action: unknown actor"))))

(define (get-next-action actor)
  (cond ((not (pc-actor? actor)) (get-next-npc-action actor))
        (else
         (get-next-pc-action))))

(define (get-pre-action-reaction action)
  (define actor (get-actor (action-actor-id action)))
  (cond ((not (pc-actor? actor))
         (cond ((equal? (actor-name actor) "Grabberkin")
                (get-grabberkin-reaction actor))
               ((equal? (actor-name actor) "Blindscraper")
                (get-blindscraper-reaction actor))
               (else
                (displayln "unknown non-pc-actor type for reaction")
                '())))
        (else
         ; (displayln "PC REACTION")
         '())))

(define (get-post-action-reaction action result)
  (define actor (action-target action))
  ; this is a chance for the target of an already-resolved action to react
  '())

(define (update-npc-reactions pc-action)
  (define npcs (get-current-enemies))
  (when (and (aggressive? pc-action)
             (not (in-combat?)))
    ; remove own actions from queue
    (for ([actor npcs])
      (define actions (filter
                       (λ (action) (eq? (actor-id actor) (action-actor-id action)))
                       action-queue))
      (remove-from-action-queue actions)
      ; blam blam
      #;(define action (make-shoot-action actor))
      (define action '())
      (add-to-action-queue action (get-action-initiative action actor)))))

; TODO: Figure out where to put this!
(define (get-action-initiative action actor)
  (dev-note "Find a place for this!")
  (define dexterity-mod (get-attribute-modifier-for (actor-dexterity actor)))

  (define action-mod 0)

  (cond ((has-tag? action 'fast)
         (set! action-mod 2))
        ((has-tag? action 'slow)
         (set! action-mod -4)))

  (define dice-1 (d 1 6))
  (define dice-2 (d 1 6))

  (define total (+ dice-1 dice-2 action-mod dexterity-mod))
  total)