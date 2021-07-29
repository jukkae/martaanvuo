#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "utils.rkt")

(lazy-require
 ["actor.rkt"
  (take-damage)])

(lazy-require
 ["situation.rkt"
  (display-combatant-info)])

(serializable-struct
 condition
 (type
  (details #:mutable)
  ; on-end-round! ; lambda ; doesn't serialize
  ))

(define (process-condition-on-end-turn owner condition)
  ; TODO: Other conditions
  (define bleed-damage-roll (d 1 6)) ; could give bonus from constitution here? say, 1d6?
  (cond ((= 1 bleed-damage-roll)
         (displayln "[Bleed check: 1d6 = 1: [1] => 1 dmg]")
         (take-damage owner 1 'bleed)
         (display-combatant-info owner)
         )
        (else
         (displayln (string-append "[Bleed check: 1d6 = 1: ["
                                   (number->string bleed-damage-roll)
                                   "]]")))))