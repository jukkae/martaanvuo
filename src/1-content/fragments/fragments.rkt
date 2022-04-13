#lang at-exp racket

(provide (all-defined-out))

(require "../../0-engine/0-api/api.rkt")

(fragment 'narrow-bridge

          (thunk
           (p @~a{
 Otava comes to a narrow, rickety-looking bridge. This is it, then, the narrow bridge – the locals say that after this there's no turning back, that wot crosses the bridge, they won't come back.
 }))

          #:decisions
          (list
           (make-decision
            #:title "Cross the narrow bridge."
            #:description "Otava crosses the bridge. It creaks and whines, but doesn't break."
            #:next-fragment 'exit
            )
           (make-decision
            #:title "Turn back."
            #:description "Otava turns back."
            #:next-fragment 'turn-back-from-narrow-bridge
            )
           ))
