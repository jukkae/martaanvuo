#lang racket

(provide (all-defined-out))

(require "../api.rkt")

(fragment
 'anthill-1
 (proc ; if fragment were a macro then this wrapping could be done automgically... but it's not trivial
  (paragraph
   "The anthill is huge. It is much taller than Otava, and it is bustling. Workers transporting raw materials, preparing, cutting, gluing, constructing. Others are tending to their crops and livestock. An orderly platoon of soldiers marches round the base. When Otava gets closer, structure emerges from the chaos of thousands of ants, and the anthill is talking to her.")
  (paragraph
   "\"BEGIN-COMMUNICATION. WHAT-IS: its type / identity?\", Anthill asks.")
  (paragraph
   "\"human / it-calls-itself Otava.\"")
  (paragraph
   "\"WHAT-IS: its goal?\""))

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
 (proc
  (paragraph
   "\"NEED: 1 grabberkin-finger, PAYMENT: 29 human-units gold\", Anthill says.")
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
   #:title "A sudden realization does not hit her. TODO: set intelligence here if not set yet, otherwise set flag for meta"
   #:description "Otava gets back to the trail, without really questioning having a conversation with an anthill."
   #:next-fragment 'exit
   )))

(fragment
 'anthill-help
 (proc
  (paragraph
   "anthill-help frag tbd.")
  )
 #:decisions
 '())


(fragment
 'anthill-passage
 (proc
  (paragraph
   "anthill-passage frag tbd.")
  )

 #:decisions
 '())

(fragment
 'anthill-monograph
 (proc
  (paragraph
   "anthill-monograph frag tbd.")
  )

 #:decisions
 '())