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
Seppo the Blacksmith buys and sells sense organs. Prices are as follows:
- Ears         1
- sonar 2
- Eyes         3
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
        #:requirement (λ () (any->bool (pc-has-sense-organ? 'sonar)))
        #:title "Sell sonar. [+2 gold]"
        #:next-fragment
        (λ ()
          (increase-pc-money! 2)
          (remove-sense-organ! 'sonar)
          'exit
          )
        )
       (make-decision
        #:requirement (λ () (any->bool (pc-has-sense-organ? 'ears)))
        #:title "Sell ears. [+1 gold]"
        #:next-fragment
        (λ ()
          (increase-pc-money! 1)
          (remove-sense-organ! 'ears)
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
        (λ () (and (not (any->bool (pc-has-sense-organ? 'sonar)))
                   (pc-has-money 2)))
        #:title "Buy sonar. [-2 gold]"
        #:next-fragment
        (λ ()
          (decrease-pc-money! 2)
          (add-sense-organ! (SenseOrgan 'sonar "sonar"))
          'exit
          )
        )
       (make-decision
        #:requirement
        (λ () (and (not (any->bool (pc-has-sense-organ? 'ears)))
                   (pc-has-money 1)))
        #:title "Buy ears. [-1 gold]"
        #:next-fragment
        (λ ()
          (decrease-pc-money! 1)
          (add-sense-organ! (SenseOrgan 'ears "ears"))
          'exit
          )
        )
       (make-decision
        #:title "Nevermind."
        #:next-fragment
        'exit
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
