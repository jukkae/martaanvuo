#lang at-exp racket

(provide (all-defined-out))

(require "../../0-engine/0-api/api.rkt")

(fragment
 'spontaneous-brain
 (thunk
  (p @~a{
"Where did the world come from? Who knows, maybe a brain spontaneously arose in the vacuum of the space from random quantum fluctuations, with such a physical state that corresponds to this mindstate that you're having right now."
   })
  )

 #:decisions
 (list
  (make-decision
   #:title "\"...\""
   #:description "\"...\""
   #:next-fragment 'exit
   )
  ))