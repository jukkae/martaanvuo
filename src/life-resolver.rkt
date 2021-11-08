#lang racket

(provide (all-defined-out))

(require "actor.rkt")
(require "io.rkt")
(require "pc.rkt")
(require "round-resolver/round-resolver.rkt")
(require "run-resolver.rkt")
(require "state/state.rkt")
(require "utils.rkt")
(require "world.rkt")


(define (resolve-life mode)
  (case mode
    ['begin (on-begin-life)]
    ['restart (on-begin-life)])

  (let/ec end-life
    (let loop ([m mode]
               [first-run? #t])

      (dev-note "FIRST RUN:")
      (displayln first-run?)
      
      (define run-exit-status
        (resolve-run m #:suppress-new-chapter? (not first-run?)))
      
      (when (eq? run-exit-status 'pc-dead)
        (on-end-life)
        (end-life 'pc-dead))
      (when (eq? run-exit-status 'win-game) (end-life 'win-game))
      (when (eq? run-exit-status 'end-run) (loop 'begin #f)) ; not first run
      (when (eq? run-exit-status 'recurse) (loop 'recurse #f)) ; not first run
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
  (let ([body 
         (list 
          (list " Round "
                (string-append " " 
                               (number->string (current-round))
                               " "))
          (list " XP "
                (string-append " " 
                               (number->string (pc-actor-xp (pc)))
                               " ")))]
        [title "Life summary"])
    (info-card body title))
  (wait-for-confirm))
 