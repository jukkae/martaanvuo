#lang at-exp racket

(provide (all-defined-out))

(require
  "../2-run-resolver/run-resolver.rkt"
  "../3-round-resolver/round-resolver.rkt"

  "../../2-core/io.rkt"
  "../../2-core/core.rkt"
  "../../2-core/session.rkt"

  "../../3-types/pc-actor.rkt"

  "../../4-systems/pc/pc.rkt"

  "../../7-state/state.rkt"

  "../../../1-content/narration/life-resolver.rkt"
  )


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

; the world and reality stays intact, world state persists
; eventually, Otava should become aware of the loop
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
  (display-end-of-life-summary)
  (wait-for-confirm))
