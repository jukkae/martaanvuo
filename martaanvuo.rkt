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










; content should be provided "somewhere"
; content is game-specific, not engine stuff
; but figure out where this should be called from
; engine / run-resolver?
(define (narrate-begin-run)
  (info-card
   (list
    (list " run " (string-append " " (number->string (situation-run *situation*)) " ")))
   (string-append "Begin run number " (number->string (situation-run *situation*))))
  (case (situation-run *situation*)
    [(1)
     (paragraph "After a couple of days of following an old blacktop road, Otava reaches the end of Edgeflats. Before her lie the vast swamplands surrounding Martaanvuo. The Collector told her of a pre-Rains laboratory, abandoned and forgotten. Rumor has it, there's a small reactor in the sub-basement, and with luck, Otava will find enough U-235 to settle her debt to the Collector.")]
    [(2)
     (paragraph "As the path descends, temperature climbs, and Otava soon finds herself drenched in sweat.")]))


; engine / run-resolver?
(define (on-begin-run)
  (set-situation-run! *situation* (add1 (situation-run *situation*)))
  (set-situation-round! *situation* 0)
  (move-actor-to-location! (situation-pc *situation*) edgeflats)
  (narrate-begin-run)
  (go-to-story-fragment 1)
  )

; engine / run-resolver?
(define (resolve-a-run)
  (on-begin-run)
  (let/ec end-run
    (let loop ()
      (define round-exit-status (resolve-round))
      (when (eq? round-exit-status 'pc-dead) (end-run 'pc-dead))
      (when (eq? round-exit-status 'win-game) (end-run 'win-game))
      (when (eq? round-exit-status 'end-run) (end-run 'end-run))
      (loop))
    ))

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