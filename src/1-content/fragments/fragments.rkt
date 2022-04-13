#lang at-exp racket

(provide (all-defined-out))

(require "../../0-engine/0-api/api.rkt")

(fragment 'narrow-bridge
          (thunk
           (p @~a{
 Otava comes to a narrow, rickety-looking bridge. This is it, then, the narrow bridge – the locals say that after this there's no turning back, that wot crosses the bridge, they won't come back.
 }))
          #:time-taken-by-fragment 2
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

(fragment 'turn-back-from-narrow-bridge
          (thunk
           (p @~a{
 Nah. There's something else, she thinks. She'll figure something out.

 Wrong on both counts: There isn't, she doesn't. And, while she was away, things got worse, much worse, and they needed to send a message.
 })
 (wait-for-confirm)
 (kill-pc! 'turned-to-a-message)
 )
          #:decisions
          '())
