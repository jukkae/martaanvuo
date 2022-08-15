#lang at-exp racket

(provide (all-defined-out))

(require "../../0-engine/0-api/api.rkt")

(define (any->bool v)
  (if v
    #t
    #f))

(fragment
 'seppo
 @~a{
Swap sensory outfit?
}

 #:time-taken-by-fragment 2
 #:decisions
 (list (make-decision
        #:requirement (λ () (any->bool (pc-has-sense-organ? 'eyes)))
        #:title "Sell eyes. [+3 gold]"
        #:next-fragment
        (λ ()
          (increase-pc-money! 3)
          (remove-sense-organ! 'eyes)
          'exit
          )
        )
       (make-decision
        #:requirement (λ () (any->bool (pc-has-sense-organ? 'echolocation)))
        #:title "Sell echolocation. [+2 gold]"
        #:next-fragment
        (λ ()
          (increase-pc-money! 2)
          (remove-sense-organ! 'echolocation)
          'exit
          )
        )
       (make-decision
        #:requirement
        (λ () (and (not (any->bool (pc-has-sense-organ? 'eyes)))
                   (pc-has-money 3)))
        #:title "Buy eyes. [-3 gold]"
        #:next-fragment
        (λ ()
          (decrease-pc-money! 3)
          (add-sense-organ! (SenseOrgan 'eyes "eyes"))
          'exit
          )
        )
       (make-decision
        #:requirement
        (λ () (and (not (any->bool (pc-has-sense-organ? 'echolocation)))
                   (pc-has-money 2)))
        #:title "Buy echolocation. [-2 gold]"
        #:next-fragment
        (λ ()
          (decrease-pc-money! 2)
          (add-sense-organ! (SenseOrgan 'echolocation "echolocation"))
          'exit
          )
        )
       )
 )

(fragment
 'spontaneous-brain
 @~a{
 "Where did the world come from? Who knows, maybe a brain spontaneously arose in the vacuum of the space from random quantum fluctuations, with such a physical state that corresponds to this mindstate that you're having right now."
}

 #:decisions
 (list
  (make-decision
   #:title "\"...\""
   #:description "\"...\""
   #:next-fragment 'exit
   )
  )
 )
