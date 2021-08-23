#lang racket

(provide (all-defined-out))
(provide (all-from-out "fragment-handler.rkt"
                       "ui.rkt"))
(provide print-meta-commands-with-keys
         meta-command-valid?)

(require racket/serialize)

(require lens)

(require "../action-resolver.rkt")
(require "../action.rkt")
(require "../actions.rkt")
(require "../actor.rkt")
(require "../blindscraper.rkt")
(require "../character-sheet.rkt")
(require "../choice.rkt")
(require "../condition.rkt")
(require "../decision.rkt")
(require "../fragment.rkt")
(require "../fragments.rkt")
(require "../grabberkin.rkt")
(require "../io.rkt")
(require "../item.rkt")
(require "../location.rkt")
(require "../locations.rkt")
(require "../pc.rkt")
(require "../quest.rkt")
(require "../round-summary.rkt")
(require "../route.rkt")
(require "../situation.rkt")
(require "../time.rkt")
(require "../utils.rkt")
(require "../world.rkt")

(require "action-queue.rkt"
         "ai.rkt"
         "event.rkt"
         "fragment-handler.rkt"
         "get-next-pc-action.rkt"
         "pc-action-resolver.rkt"
         "round.rkt"
         "simulation.rkt"
         "timeline.rkt"
         "ui.rkt")


; engine / round resolver
(define (enqueue-npc-actions)
  (define actors (location-actors (current-location)))
  (for ([actor actors])
    (when (not (pc-actor? actor))
      (define next-action (get-next-action actor))
      (add-to-action-queue next-action))))

; engine / round resolver
(define (resolve-turn! world action)
  (if (pc-actor? (action-actor action))
      (resolve-pc-action! action)
      (resolve-npc-action! action))
  )




; engine / round resolver
; MAIN RESOLVER ENTRYPOINT
(define (resolve-round mode)
  (on-begin-round mode)
  
  (enqueue-npc-actions)
  
  (if (eq? mode 'continue)
      (redescribe-situation)
      (describe-situation))
  
  (save)
  (let/ec end-round-early-with-round-status
    (define pc-action (get-next-pc-action))
    
    (cond ((eq? pc-action 'end-round-early)
           (on-end-round)
           (end-round-early-with-round-status 'ok))
          ((eq? pc-action 'restart)
           (end-round-early-with-round-status 'restart))
          ((eq? pc-action 'recurse)
           (end-round-early-with-round-status 'recurse))
          ((eq? pc-action 'end-chapter)
           (on-end-round)
           (next-chapter!)
           (end-round-early-with-round-status 'ok))
          (else

           (describe-pc-intention pc-action)
  
           

           (define round-exit-status 'ok)
           (cond ((initiative-based-resolution? pc-action)
                  (add-to-action-queue pc-action)
                  (update-npc-reactions pc-action)
                  (sort-action-queue)
                  (resolve-turns!))
                 (else
                  (define pc-action-result (resolve-pc-action! pc-action))
                  (when (eq? 'end-run pc-action-result) (set! round-exit-status 'end-run))
                  (when (eq? 'win-game pc-action-result) (set! round-exit-status 'win-game))))
           (on-end-round)
           (when (not (pc-actor-alive? (pc))) (set! round-exit-status 'pc-dead))
           round-exit-status
           ))))

; engine / round resolver
(define (resolve-npc-action! action)
  (resolve-action! action))

(define (end-combat)
  (remove-all-enemies-and-end-combat!)
  (clear-action-queue!))

; engine / round resolver
(define (resolve-turns!)
  (let/ec end-round-early
    (when (all-actions-of-type? action-queue 'flee)
      (p "Otava turns her back to flee and crawls under a bush to hide. She waits a while. Nothing seems to be following her.")
      (award-xp! 1)
      (end-combat)
      (end-round-early))
    (for ([action action-queue])

      (define actor (action-actor action))
      
      (define pre-action-reaction? (get-pre-action-reaction action))
      (when (not (null? pre-action-reaction?))
        (set! action pre-action-reaction?))
      
      (define turn-result (resolve-turn! world action))

      ; todo
      (define post-action-reaction-from-target? (get-post-action-reaction action turn-result))
      (when (not (null? post-action-reaction-from-target?))
        ;(define action post-action-reaction-from-target?)
        (displayln "-- post-action-reaction-from-target?: handle!"))
      
      (case turn-result
        
        ['pc-dead
         (end-round-early)]
        
        ['end-combat
         (end-combat)
         (end-round-early)
         ]

        ; TODO: As always, description belongs in the action
        ['grip-released
         (p "The Grabberkin's hands let go of Otava's ankles and disappear under the moss.")
         (award-xp! 3 "for surviving an encounter with a Grabberkin")
         (remove-enemy actor)
         ]
        )
      )
    ))



(define (save)
  (save-situation *situation*))
