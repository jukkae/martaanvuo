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
            #:time-taken 2
            #:next-fragment 'narrow-bridge-crossed
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


(fragment 'narrow-bridge-crossed
          (thunk
          (set-flag 'narrow-bridge-crossed)
           (p @~a{
Then, there's the matter of her brother's treatments. If she can scrounge together a couple thousand, there's a doctor that knows how to fix her brother, but time's starting to run out soon. Who knows, a month? A month and a half?
 })
           (create-task 'the-treatments)
           (wait-for-confirm)
           )
          #:decisions
          '())