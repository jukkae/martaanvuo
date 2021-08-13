#lang racket

(provide (all-defined-out))

(require "io.rkt")
(require "pc.rkt")
(require "round-resolver.rkt")
(require "run-resolver.rkt")
(require "situation.rkt")


(define (resolve-life mode)
  (when (eq? mode 'begin)
    (on-begin-life))
  
  (let/ec end-life
    (let loop ([m mode])
      (define run-exit-status (resolve-run m))
      (when (eq? run-exit-status 'pc-dead) (end-life 'pc-dead))
      (when (eq? run-exit-status 'win-game) (end-life 'win-game))
      (when (eq? run-exit-status 'end-run) (loop 'begin))
      (when (eq? run-exit-status 'recurse) (loop 'recurse))
      (when (eq? run-exit-status 'restart) (end-life 'restart))
      )
    ))


(define (on-begin-life)
  (set-situation-life! *situation* (add1 (situation-life *situation*)))
  (set-situation-pc! *situation* (make-new-pc))
  (go-to-story-fragment 1)
  (when (not (= 1 (situation-life *situation*)))
    (player-info)))

(define (on-end-life)
  (displayln "[Life summary TODO]")
  ; -> serialize run statistics
  (clean-situation!)
  )