#lang at-exp racket

(provide (all-defined-out))

(require
  "../../0-engine/2-core/io.rkt"
  "../../0-engine/2-core/core.rkt"

  "../../0-engine/3-types/pc-actor.rkt"

  "../../0-engine/4-rules/pc/pc.rkt"
  "../../0-engine/4-rules/actors/actor.rkt"
  "../../0-engine/7-state/state/state.rkt"
  )

(define (display-end-of-life-summary)
  (let ([body
         (tbody
          (tr "Round"
              (format "~a" (current-round)))
          (tr "XP"
              (format "~a" (pc-actor-xp (pc)))))]
        [title "Life summary"])
    (info-card body title)))
