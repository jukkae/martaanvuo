#lang racket

(provide resolve-round)
(provide (all-from-out "fragment-handler.rkt"
                       "ui.rkt"))
(provide print-meta-commands-with-keys
         meta-command-valid?)

(require
  ; these are needed for providing
  "fragment-handler.rkt"
  "ui.rkt"

  "action-queue.rkt"
  "ai.rkt"
  "get-next-pc-action.rkt"
  "initiative-based-round-resolver.rkt"
  "round.rkt"

  "../action-resolver/action-resolver.rkt"

  "../../actions/action.rkt"
  "../../actions/pc-choices.rkt"

  "../../actors/actor.rkt"
  "../../actors/pc-actor.rkt"

  "../../core/io.rkt"
  "../../core/utils.rkt"

  "../../locations/0-types/location.rkt"

  "../../pc/pc.rkt"

  "../../state/logging.rkt"
  "../../state/state.rkt")

(define (resolve-round mode)
  (define round-begin-status (on-begin-round mode))

  (enqueue-npc-actions)

  (case round-begin-status
    ['pc-dead (unset-current-fragment-id!)])

  (save) ; save before describing situation -> no double-logged paragraphs

  (when (current-show-round-summary?)
    (if (eq? mode 'continue)
      '()#;(redescribe-situation)
      (describe-situation)))

  ; this fixes save files after death
  (when (not (pc-is-alive?))
    (set! round-begin-status 'pc-dead)

    ; This "reprints" the "Otava is dead" notice
    (when (eq? mode 'continue)
      (define cause-of-death (pc-actor-cause-of-death (pc)))
      (notice (format "Otava is dead. Cause of death: ~a"
                      (cond ((symbol? cause-of-death)
                             (describe-cause-of-death cause-of-death))
                        ((string? cause-of-death)
                         cause-of-death)
                        ((symbol? (car cause-of-death))
                         (describe-cause-of-death (car cause-of-death)))
                        ((string? (car cause-of-death))
                         (car cause-of-death))
                        (else "NA"))))))

  (case round-begin-status
    ['pc-dead
      'pc-dead]

    [else
      ; chonky boi, extract function
      (let/ec end-round-early-with-round-status
        (define pc-action (get-next-pc-action))

        (cond
              ((eq? pc-action 'end-round-early)
               (on-end-round)
               (end-round-early-with-round-status 'ok))
              ((eq? pc-action 'pc-dead)
               (on-end-round)
               (end-round-early-with-round-status 'pc-dead))
              ((eq? pc-action 'restart)
               (on-end-round)
               (end-round-early-with-round-status 'restart))
              ((eq? pc-action 'recurse)
               (on-end-round)
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
                       (define pc-action-result (resolve-action! pc-action))
                       (when (eq? 'end-run pc-action-result) (set! round-exit-status 'end-run))
                       (when (eq? 'win-game pc-action-result) (set! round-exit-status 'win-game))))

               (on-end-round)
               (when (not (pc-actor-alive? (pc))) (set! round-exit-status 'pc-dead))
               round-exit-status)))]))

(define (enqueue-npc-actions)
  (define actors (location-actors (current-location)))
  (for ([actor actors])
    (when (not (pc-actor? actor))
      (define next-action (get-next-action actor))
      (add-to-action-queue next-action))))