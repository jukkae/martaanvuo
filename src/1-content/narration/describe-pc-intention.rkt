#lang at-exp racket

(require "../../0-engine/2-core/io.rkt"
         "../../0-engine/3-types/action.rkt")

(provide describe-pc-intention)
(define (describe-pc-intention pc-action)
  (when (not (null? pc-action)) ; should be checked at call site but eh
    (case (action-symbol pc-action)
      ['forage
       (p "Otava is getting low on supplies, and it looks like there could be bogberries around.")]
      #;[else (p "TBD")])))
