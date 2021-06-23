#lang racket

(provide (all-defined-out))

(require "pc.rkt")
(require "run-resolver.rkt")
(require "situation.rkt")


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

(define (on-begin-life)
  (set-situation-life! *situation* (add1 (situation-life *situation*)))
  (set-situation-pc! *situation* (make-new-pc))
  #;(player-info))

(define (on-end-life)
  (displayln "[Life summary TODO]")
  ; -> serialize run statistics
  (clean-situation!)
  )