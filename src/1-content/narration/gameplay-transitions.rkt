#lang at-exp racket

(provide (all-defined-out))

(require
  "../../0-engine/2-core/io.rkt"
  "../../0-engine/2-core/core.rkt"

  "../../0-engine/3-types/task.rkt"
  "../../0-engine/3-types/pc-actor.rkt"

  "../../0-engine/4-systems/blurbs/blurbs.rkt"
  "../../0-engine/4-systems/pc/pc.rkt"

  "../../0-engine/7-state/state.rkt"
  "../../0-engine/7-state/logging.rkt"
  )

(define (narrate-begin-run #:suppress-new-chapter? [suppress-new-chapter? #f])
  (when (not suppress-new-chapter?) (next-chapter!))

  ; Don't show this until the second run!
  (when (not (= 1 (current-run)))
    (notice (format "Begin run number ~a" (current-run))))
  )

(define (narrate-begin-recurse-run)
  (next-chapter!)

  (p @~a{
    Otava is on Mediator's path in the foggy cardboard cutout woods. She gets to Fork and Anthill.
  }))

(define (display-run-summary)
  (info-card
    (tbody
      (tr "run"
          (number->string (current-run)))
      (tr "gold collected"
          (number->string (pc-gold-amount))))
    (format "Run number ~a ended" (current-run))))

(define (display-end-of-life-summary)
  (let ([body
         (tbody
          (tr "Round"
              (format "~a" (current-round)))
          (tr "XP"
              (format "~a" (pc-actor-xp (pc)))))]
        [title "Life summary"])
    (info-card body title)))
