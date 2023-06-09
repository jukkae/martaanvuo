#lang at-exp racket

(provide (all-defined-out))

(require "../../0-engine/2-core/io.rkt"
         "../../0-engine/2-core/core.rkt"

         "../../0-engine/3-types/task.rkt"
         "../../0-engine/3-types/pc-actor.rkt"

         "../../0-engine/4-systems/blurbs/blurbs.rkt"
         "../../0-engine/4-systems/pc/pc.rkt"
         "../../0-engine/4-systems/world/world.rkt"

         "../../0-engine/7-state/state.rkt"
         "../../0-engine/7-state/logging.rkt")

(define (narrate-begin-run #:suppress-new-chapter? [suppress-new-chapter? #f])
  #;(when (not suppress-new-chapter?)
      #;(next-chapter!))

  #;
  (case (current-run)
    [(1)
     (p
      "The road disappears under rocks. This is it, the rest of the way is by foot. Otava parks the bike and follows a narrow path to the canyons.")]
    [(2) (p "Otava parks her bike and heads to the canyons and crosses the creaky narrow bridge.")])

  ; Don't show this until the second run!
  (when (not (= 1 (current-run)))
    (notice (format "Begin run number ~a" (current-run)))))

(define (display-run-summary)
  (info-card (tbody (tr "run" (number->string (current-run)))
                    (tr "gold collected" (number->string (pc-gold-amount))))
             (format "Run number ~a ended" (current-run))))

(define (display-end-of-life-summary)
  (let ([body (tbody (tr "Round" (format "~a" (current-round)))
                     (tr "XP" (format "~a" (pc-actor-xp (pc)))))]
        [title "Life summary"])
    (info-card body title)))
