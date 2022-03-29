#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require
  "action-initiative-resolver.rkt"

  "../../2-core/core.rkt"
  "../../3-types/action.rkt"
  "../../3-types/actor.rkt"
  "../../3-types/pc-actor.rkt"
  "../../4-rules/actors/actor.rkt"
  "../../4-rules/enemies/blindscraper.rkt"
  "../../4-rules/enemies/grabberkin.rkt"
  "../../7-state/state/state.rkt"
  )

(require "action-queue.rkt")

(lazy-require ["round-resolver.rkt"
  (get-next-pc-action
   )])
(lazy-require ["../../../4-rules/world/world.rkt"
 (get-actor
  )])


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
      (add-to-action-queue action (resolve-action-initiative action actor)))))
