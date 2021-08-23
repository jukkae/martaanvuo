#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "../action.rkt"
         "../actor.rkt"
         "../blindscraper.rkt"
         "../grabberkin.rkt")

(lazy-require
 ["round-resolver.rkt"
  (get-next-pc-action
   )])

(define (get-next-npc-action actor)
  (case (actor-name actor)
    (["Blindscraper"] (get-blindscraper-action actor))
    (["Grabberkin"] (get-grabberkin-action actor))
    (else (displayln "get-next-npc-action: unknown actor"))))

(define (get-next-action actor)
  (cond ((not (pc-actor? actor)) (get-next-npc-action actor))
        (else
         (get-next-pc-action)
         ))
  )

(define (get-pre-action-reaction action)
  (define actor (action-actor action))
  (cond ((not (pc-actor? actor))
         (cond ((equal? (actor-name actor) "Grabberkin")
                (get-grabberkin-reaction actor))
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