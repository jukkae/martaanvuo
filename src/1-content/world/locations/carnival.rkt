#lang at-exp racket

(provide (all-defined-out))

(require
  "../../../0-engine/0-api/api.rkt")

(require
  "../../../0-engine/3-types/choice.rkt"
  )

(fragment
 'bobo-the-clown-1
 (thunk
  (p @~a{
Bobo the Clown waves his arms, squeezes his honking nose, breathes in from a balloon, and shouts in a voice turned chipmunk-like by the helium

BOBO THE CLOWN:
"Slither hither! Huff a puff of Bobo's stuff! Faffing lass, have some laughing gas! It's good for you, and good for what's hurting you! You will be a brand new person! Only two dollars a huff!"
 }))
 #:decisions
 (list (make-decision
        #:title "Inhale."
        #:next-fragment 'exit
        )
       (make-decision
        #:title "Don't inhale."
        #:next-fragment 'exit
        )))

(define (get-carnival-choices)
  (list
    (make-choice
      'bobo-the-clown
      "Bobo the Clown."
      (λ ()
        (go-to-fragment 'bobo-the-clown-1)
        'end-chapter ; ie., 'end-round-early, plus next chapter on next round
        ))
    (make-choice
      'the-endless-staircase
      "The Endless Staircase."
      (λ ()
        (go-to-fragment 'endless-staircase-1)
        'end-chapter ; ie., 'end-round-early, plus next chapter on next round
        ))
    (make-choice
      'the-endless-staircase
      "The Merchant's Bio-Mechanical Emporium of Oddities."
      (λ ()
        (go-to-fragment 'the-other-merchant-1)
        'end-chapter ; ie., 'end-round-early, plus next chapter on next round
        ))
  ))

(define (get-carnival-forbidden-actions)
  (list 'rest))