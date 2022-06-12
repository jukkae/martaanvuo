#lang at-exp racket

(provide (all-defined-out))

(require "../../0-engine/0-api/api.rkt")
(require "../../0-engine/6-combat/combat.rkt")

(fragment
 'post-combat ; for instance, wound care (fast vs good), xp, summary etc
  "[post-combat steps]"
 #:on-before-describe!
  (thunk
    (display-pc-combatant-info (pc)))

 #:decisions
 (append
  (list (make-decision
         #:title "Don't treat wounds."
         #:description "Combat finished."
         #:next-fragment 'exit
         )
        )
  ))

