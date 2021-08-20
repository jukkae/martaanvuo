#lang racket

(provide (all-defined-out))

(require "../api.rkt")

(fragment
 'anthill-1
 
 (proc
  (p
   "The anthill is huge. It is much taller than Otava, and it is bustling. Workers transporting raw materials, preparing, cutting, gluing, constructing. Others are tending to their crops and livestock. An orderly platoon of soldiers marches round the base. When Otava gets closer, structure emerges from the chaos of thousands of ants, and the anthill is talking to her.")
  (p
   "\"BEGIN-COMMUNICATION. WHAT-IS: its type / identity?\", Anthill asks.")
  (p
   "\"human / it-calls-itself Otava.\"")
  (p
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
  (p "Otava walks away from the anthill, and it is only when she's halfway back to the trail when it hits her: She has no clue how, exactly, the thoughts of the anthill entered her mind. It wasn't writing, and anthills cannot talk.")
  (p "She plods through the swamp back to the anthill. It is not nearly as big as she thought it was, and it is definitely not talking to her. Dismayed, she returns to the trail"))

(fragment
 'anthill-work
 (proc
  (p
   "\"NEED: 1 grabberkin-finger, PAYMENT: 29 human-units gold\", Anthill says.")
  (create-quest 'grabberkin-finger)
  (p "\"REQUEST: information <subject: grabberkin>\", Otava asks.")
  (p "\"GRABBERKIN: envious drowner-bonebreakers, water-dweller slowmovers\", Anthill replies, \"rotten strong-grips.\"")
  (p "\"REQUEST: information <subject: grabberkin-finger, uses>\", Otava continues.")
  (p "\"DENIED: not-relevant\", Anthill replies. \"END-COMMUNICATION\", it concludes, and the ants go back to work.")
  (p "She's a bit taken aback by the abruptly ended conversation. Still, 29 grams of gold is good news.")
  )

 #:decisions
 (list
  (make-decision
   #:title "A sudden realization hits her."
   #:description "Otava realizes that she has no clue how, exactly, did the anthill talk to her. It wasn't writing, and anthills cannot speak. She decides to circle back to the anthill, but it is not nearly as big as she thought it was, and it is definitely not talking to her."
   #:next-fragment 'exit ; this should also take some time -> risky!
   )
  (make-decision
   #:title "A sudden realization does not hit her."
   #:description "Otava gets back to the trail." ; TODO: here, give some meta-reward (or maybe just *not* be risky)
   #:next-fragment 'exit
   )))

(fragment
 'anthill-help
 (proc
  (p
   "anthill-help frag tbd.")
  )
 #:decisions
 '())


(fragment
 'anthill-passage
 (proc
  (p
   "anthill-passage frag tbd.")
  )

 #:decisions
 '())

(fragment
 'anthill-monograph
 (proc
  (p
   "anthill-monograph frag tbd.")
  )

 #:decisions
 '())