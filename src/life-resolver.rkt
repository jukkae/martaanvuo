#lang racket

(provide (all-defined-out))

(require "io.rkt")
(require "pc.rkt")
(require "round-resolver/round-resolver.rkt")
(require "run-resolver.rkt")
(require "state/state.rkt")
(require "utils.rkt")
(require "world.rkt")


(define (resolve-life mode)
  (when (eq? mode 'begin)
    (on-begin-life))
  
  (let/ec end-life
    (let loop ([m mode])
      (define run-exit-status (resolve-run m))
      (when (eq? run-exit-status 'pc-dead)
        (on-end-life)
        (end-life 'pc-dead))
      (when (eq? run-exit-status 'win-game) (end-life 'win-game))
      (when (eq? run-exit-status 'end-run) (loop 'begin))
      (when (eq? run-exit-status 'recurse) (loop 'recurse))
      (when (eq? run-exit-status 'restart) (end-life 'restart))
      )
    ))


(define (on-begin-life)
  (current-life (add1 (current-life)))
  (current-pc (make-new-pc))
  (go-to-story-fragment 'begin-life)
  (when (not (= 1 (current-life)))
    (player-info)))

(define (on-end-life)
  (let ([body (list (list " a " " b "))]
        [title "Life summary"])
    (info-card body title))
  (wait-for-confirm))
 