#lang at-exp racket

(provide resolve-round)
(provide (all-from-out "fragment-handler.rkt"))

(require
  "fragment-handler.rkt"
  "action-initiative-resolver.rkt"
  "action-queue.rkt"
  "ai.rkt"
  "initiative-based-round-resolver.rkt"
  "round.rkt"

  "get-next-pc-action/get-next-pc-action.rkt"
  "get-next-pc-action/pc-choices.rkt"
  "get-next-pc-action/ui.rkt"

  "../2-action-resolver/action-resolver.rkt"

  "../../2-core/io.rkt"
  "../../2-core/core.rkt"

  "../../3-types/action.rkt"
  "../../3-types/item.rkt"
  "../../3-types/pc-actor.rkt"
  "../../3-types/location.rkt"
  "../../3-types/place.rkt"
  "../../3-types/route.rkt"

  "../../4-systems/actors/actor.rkt"
  "../../4-systems/pc/pc.rkt"

  "../../7-state/logging.rkt"
  "../../7-state/state.rkt"

  "../../../1-content/narration/describe-pc-intention.rkt"
  )

; Returns round-result:
; (U 'ok 'pc-dead)
(define (resolve-round mode)
  (define round-begin-status (on-begin-round mode))

  (case round-begin-status
    ['pc-dead (clear-current-fragment!)])
  (save) ; save before describing situation -> no double-logged paragraphs

  (when (current-show-round-summary?)
    (if (equal? mode 'continue)
        '()#;(describe-situation #t)
        '()#;(describe-situation #f)))

  (when (and (pc-has-item? 'lucky-charm-slot-machine)
             (equal? (item-details (pc-has-item? 'lucky-charm-slot-machine)) 'active))
    (define n (ephemeral-random-in-range 111 999))
    (p (format "\"Bu-di-du-duh.\" The slot machine sings a little melody, and then bloinks and whirrs. Chunk-chunk-chunk, the numbers lock into place. ~a." n))
    (for ([i n])
      (random)))

  ; this fixes save files after death
  (when (not (pc-is-alive?))
    (set! round-begin-status 'pc-dead)

    ; This "reprints" the "Otava is dead" notice
    (when (equal? mode 'continue)
      (define cause-of-death (pc-actor-cause-of-death (pc)))
      (notice (format "Otava is dead. Cause of death: ~a"
                      (cond
                        [(empty? cause-of-death)
                         "NA"]
                        [(symbol? cause-of-death)
                         (describe-cause-of-death cause-of-death)]
                        [(string? cause-of-death)
                         cause-of-death]
                        )))))

  (case round-begin-status
    ['pc-dead
     'pc-dead]

    [else
     (let/ec end-round-early-with-round-status
       (enqueue-npc-actions)
       (define pc-action (get-next-pc-action))
       (resolve-pc-action pc-action end-round-early-with-round-status) )]))

(define (resolve-pc-action pc-action end-round-early-with-round-status)
  (cond
    ((equal? pc-action 'end-round-early)
     (on-end-round)
     (end-round-early-with-round-status 'ok))
    ((equal? pc-action 'pc-dead)
     (on-end-round)
     (end-round-early-with-round-status 'pc-dead))
    ((equal? pc-action 'restart)
     (on-end-round)
     (end-round-early-with-round-status 'restart))
    ((equal? pc-action 'recurse)
     (on-end-round)
     (end-round-early-with-round-status 'recurse))
    ((equal? pc-action 'end-chapter)
     (on-end-round)
     (next-chapter!)
     (end-round-early-with-round-status 'ok))

    (else
     (describe-pc-intention pc-action)
     (define round-exit-status 'ok)
     (cond ((initiative-based-resolution? pc-action)
            (add-to-action-queue pc-action (resolve-action-initiative pc-action (pc)))
            (update-npc-reactions pc-action)
            (sort-action-queue)
            (resolve-turns!))
           (else
            (define pc-action-result (resolve-action! pc-action))
            (when (equal? 'end-run pc-action-result) (set! round-exit-status 'end-run))
            (when (equal? 'win-game pc-action-result) (set! round-exit-status 'win-game))))

     (on-end-round)
     (when (not (pc-actor-alive? (pc))) (set! round-exit-status 'pc-dead))
     round-exit-status)))

(define (enqueue-npc-actions)
  (define actors (location-actors (current-location)))
  (for ([actor actors])
    (when (not (pc-actor? actor))
      (define next-action (get-next-action actor))
      (add-to-action-queue next-action (resolve-action-initiative next-action actor)))))
