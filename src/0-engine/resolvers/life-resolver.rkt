#lang at-exp racket

(provide (all-defined-out))

(require
  "run-resolver.rkt"

  "round-resolver/round-resolver.rkt"

  "../actors/actor.rkt"

  "../core/io.rkt"
  "../core/session.rkt"
  "../2-core/core.rkt"


  "../pc/pc.rkt"
  "../state/state.rkt")


(define (resolve-life mode)
  (case mode
    ['begin (on-begin-life)]
    ['restart (on-begin-life)])

  (let/ec end-life
    (let loop ([m mode])

      ; first run of the life
      (define first-run?
        (case m
          ['begin #t]
          ['restart #t]
          ['else #f]))

      (define run-exit-status
        (resolve-run m #:suppress-new-chapter? (not first-run?)))

      (case run-exit-status
        ['pc-dead (on-end-life)
         (end-life 'pc-dead)]
        ['win-game (end-life 'win-game)]
        ['end-run (loop 'begin)]
        ['recurse (loop 'recurse)]
        ['restart (end-life 'restart)])
      )
    ))

(define (on-begin-life)
  (when (not (session-flag-set? 'began-life))
    (set-session-flag 'began-life)
    (current-session-score-dice++)
    )

  (current-life (add1 (current-life)))
  (current-pc (make-new-pc))
  (set-base-build!)
  (go-to-fragment 'begin-life)
  (when (not (= 1 (current-life)))
    (dev-note "Show life info")))

(define (on-end-life)
  (let ([body
         (tbody
          (tr "Round"
              (format "~a" (current-round)))
          (tr "XP"
              (format "~a" (pc-actor-xp (pc)))))]
        [title "Life summary"])
    (info-card body title))
  (wait-for-confirm))
