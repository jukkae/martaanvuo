#lang at-exp racket

(provide (all-defined-out))

(require "../../core/api.rkt")

(fragment
 'anthill-1

 (thunk
 (p @~a{
    The anthill is huge. It is much taller than Otava, and it is bustling. Workers transporting raw materials, preparing, cutting, gluing, constructing. Others are tending to their crops and livestock. An orderly platoon of soldiers marches round the base. When Otava gets closer, structure emerges from the chaos of thousands of ants, and the anthill is talking to her.

    "BEGIN-COMMUNICATION. WHAT-IS: its type / identity?", Anthill asks.

    "human / it-calls-itself Otava the Seeker.", Otava responds.

    "CAUSE : Otava the Seeker BRINGS 5x GRABBERKIN FINGER
     EFFECT: 1 GRAM OF GOLD
     AGREE?", Anthill asks.
  }))

 #:decisions
 (list
  (make-decision
   #:title "Agree."
   #:description "\"AGREE.\", Otava agrees."
   #:next-fragment 'anthill-grabberkin-fingers
   )
  (make-decision
   #:title "Ask for more. [charisma check]"
   #:description "\"AGREE.\""
   #:next-fragment 'anthill-grabberkin-fingers
   )
  ))

(fragment
 'anthill-grabberkin-fingers
 (thunk
  (create-task 'grabberkin-fingers)
  ))
