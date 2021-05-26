#lang racket

(require racket/generator)
(require racket/serialize)

(require lens)
(require text-table)

(require "action-resolver.rkt")
(require "action.rkt")
(require "actions.rkt")
(require "actor.rkt")
(require "blindscraper.rkt")
(require "checks.rkt")
(require "character-sheet.rkt")
(require "fragment.rkt")
(require "fragments.rkt")
(require "grabberkin.rkt")
(require "io.rkt")
(require "location.rkt")
(require "pc.rkt")
(require "round-resolver.rkt")
(require "run-resolver.rkt")
(require "situation.rkt")
(require "utils.rkt")
(require "world.rkt")



; game-specific, not engine!
(define (title)
  (newline)
  (displayln "M A R T A A N V U O")
  (displayln "===================")
  (newline))


; this definition looks like it should happen at the call site
; - expose-neighbor! should be available for scripting
(provide handle-exploration-check-result!)
(define (handle-exploration-check-result! result)
  (if result
      (begin
        (expose-neighbor! (current-location))
        'successful)
      (begin
        (displayln "Exploration failed.")
        'failure)))




; engine / life-resolver?
(define (resolve-a-life)
  (on-begin-life)
  (let/ec end-life
    (let loop ()
      (define run-exit-status (resolve-a-run))
      (when (eq? run-exit-status 'pc-dead) (end-life 'pc-dead))
      (when (eq? run-exit-status 'win-game) (end-life 'win-game))
      (when (eq? run-exit-status 'end-run)
        (displayln "TODO: END RUN")
        (loop)))
    ))

; engine / life-resolver?
(define (on-begin-life)
  (set-situation-life! *situation* (add1 (situation-life *situation*)))
  (set-situation-pc! *situation* (make-new-pc))
  (player-info)  
  )

; engine / game-resolver? meta player.rkt?
(define (on-begin-playthrough)
  ;(paragraph "[" "Begin a story" "]")
  (setup-world)
  )

; engine / game-resolver?
(define (begin-game)
  #; (random-seed 13)
  (title)
  (on-begin-playthrough)
  (let/ec win-game
    (let begin-new-life ()
      (define pc-life-end-status (resolve-a-life))
      (when (eq? pc-life-end-status 'pc-dead)

        (let end-of-life-menu ([verbosity 'verbose])
          (define (handle-meta-command meta-commands-with-keys input)
            (set! input (string-upcase input))
            (define meta-command-with-key (hash-ref meta-commands-with-keys input '()))
            (define meta-command (cdr meta-command-with-key))
            (meta-command)
            (end-of-life-menu 'verbose))

          (define meta-commands (make-hash))
          (hash-set! meta-commands "Q" (cons "[Q]: Quit." quit))
          (hash-set! meta-commands "R" (cons "[R]: Reincarnate." begin-new-life)) ; TODO: change prompt dynamically based on meta-progress

          (paragraph "Reincarnate?")
          (print-meta-commands-with-keys meta-commands)
          (define input (wait-for-input))
          (serialize-input)

          (newline)

          (cond ((meta-command-valid? meta-commands input) (handle-meta-command meta-commands input))
                (else (end-of-life-menu 'abbreviated)))))
      (when (eq? pc-life-end-status 'win-game) (win-game))))
  (win-game)
  
  )

; scripting API / game-specific
(define (win-game)
  (paragraph "Otava dives under the waters of Martaanvuo Spring and forever ceases to exist.")
  (wait-for-input)
  (exit))

; main entrypoint
(begin-game)