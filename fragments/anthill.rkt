#lang racket

(provide (all-defined-out))

(require "../fragment.rkt")
(require "../decision.rkt")
(require "../situation.rkt")
(require "../io.rkt")
(require "../quests.rkt")

(fragment
 'anthill-1
 (string-append
  "The anthill is huge. It is much taller than Otava, and it is bustling. Workers transporting raw materials, preparing, cutting, gluing, constructing. Others are tending to their crops and livestock. An orderly platoon of soldiers marches round the base. When Otava gets closer, structure emerges from the chaos of thousands of ants, and the anthill is talking to her."
  "\n\n"
  "\"BEGIN-COMMUNICATION. WHAT-IS: its type / identity?\", Anthill asks."
  "\n\n"
  "\"human / it-calls-itself Otava.\""
  "\n\n"
  "\"WHAT-IS: its goal?\"")

 #:decisions
 (list
  (make-decision
   #:title "GOAL: gold, REQUEST: work."
   #:description "\"GOAL: gold. REQUEST: work-for-gold <or: equivalent>\""
   #:next-fragment 'anthill-work
   )
  (make-decision
   #:title "GOAL: freedom, REQUEST: help."
   #:description "\"GOAL: freedom. REQUEST: help <or: information, or: equivalent>\""
   #:next-fragment 'anthill-help
   )
  (make-decision
   #:title "GOAL: secret, REQUEST: passage."
   #:description "\"GOAL: redacted. REQUEST: passage.\""
   #:next-fragment 'anthill-passage
   )
  (make-decision
   #:title "GOAL: anthead-monograph, REQUEST: information."
   #:description "\"GOAL: anthead-monograph. REQUEST: information.\""
   #:next-fragment 'anthill-monograph
   #:requirement (λ () (quest-exists? 'anthead-monograph))
   )
  ))

(define (anthill-print-end-conversation)
  (paragraph "Otava walks away from the anthill, and it is only when she's halfway back to the trail when it hits her: She has no clue how, exactly, the thoughts of the anthill entered her mind. It wasn't writing, and anthills cannot talk.")
  (paragraph "She plods through the swamp back to the anthill. It is not nearly as big as she thought it was, and it is definitely not talking to her. Dismayed, she returns to the trail"))

(fragment
 'anthill-work
 (string-append
  "\"NEED: 1 grabberkin-finger, PAYMENT: 29 human-units gold\", Anthill says.")
 #:on-begin-round!
 (λ ()
   (create-quest 'grabberkin-finger)
   (paragraph "\"REQUEST: information <subject: grabberkin>\", Otava asks.")
   (paragraph "\"GRABBERKIN: envious drowner-bonebreakers, water-dweller slowmovers\", Anthill replies, \"rotten strong-grips.\"")
   (paragraph "\"REQUEST: information <subject: grabberkin-finger, uses>\", Otava continues.")
   (paragraph "\"DENIED: not-relevant\", Anthill replies. \"END-COMMUNICATION\", it concludes, and the ants go back to work.")
   )
 #:decisions
 (list
  (make-decision
   #:title "A sudden realization hits her."
   #:description "Almost half the way back, Otava realizes that she has no clue how exactly the anthill talked to her. It wasn't writing, and anthills cannot talk. She walks back to the anthill, but it is not nearly as big as she thought it was, and it is definitely not talking to her."
   #:next-fragment 'exit
   )
  (make-decision
   #:title "A sudden realization does not hit her."
   #:description "Otava gets back to the trail, without really questioning having a conversation with an anthill."
   #:next-fragment 'exit
   )))

(fragment
 'anthill-help
 (string-append
  "\"NEED: 1 grabberkin-finger, PAYMENT: 29 human-units gold\", Anthill says.")
 #:on-begin-round!
 (λ ()
   (create-quest 'grabberkin-finger)
   (paragraph "\"REQUEST: information <subject: grabberkin>\", Otava asks.")
   (paragraph "\"GRABBERKIN: envious drowner-bonebreakers, water-dweller slowmovers\", Anthill replies, \"rotten strong-grips.\"")
   (paragraph "\"REQUEST: information <subject: grabberkin-finger, uses>\", Otava continues.")
   (paragraph "\"DENIED: not-relevant\", Anthill replies. \"END-COMMUNICATION\", it concludes, and the ants go back to work.")
   (anthill-print-end-conversation))
 #:decisions
 '())


(fragment
 'anthill-passage
 (string-append
  "anthill-passage")

 #:decisions
 '())

(fragment
 'anthill-monograph
 (string-append
  "anthill-monograph")

 #:decisions
 '())