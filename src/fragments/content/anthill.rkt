#lang at-exp racket

(provide (all-defined-out))

(require "../../core/api.rkt")

(fragment 'anthill-1

          (thunk
           (p @~a{
 The anthill is huge. It is much taller than Otava, and it is bustling. Workers transporting raw materials, preparing, cutting, gluing, constructing. Others are tending to their crops and livestock. An orderly platoon of soldiers marches round the base. When Otava gets closer, structure emerges from the chaos of thousands of ants, and the anthill is talking to her.

 "BEGIN-COMMUNICATION. WHAT-IS: its type / identity?", Anthill asks.

 "human / it-calls-itself Otava the Seeker.", Otava responds.

 "CAUSE : Otava the Seeker BRINGS 1 BUNDLE OF 5 GRABBERKIN FINGERS
 EFFECT: 2 GRAM OF GOLD
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
            #:title "Inquire about Grabberkin."
            #:description "\"INQUIRY: grabberkin: WHAT-IS: its type, features, etc?\", Otava asks."
            #:next-fragment 'anthill-grabberkin-inquiry
            #:requirement (λ () (not (fragment-completed? 'anthill-grabberkin-inquiry)))
            )
           (make-decision
            #:title "Inquire about Grabberkin fingers."
            #:description "\"INQUIRY: USES-FOR etc: grabberkin finger?\", Otava asks."
            #:next-fragment 'anthill-grabberkin-fingers-inquiry
            #:requirement (λ () (not (fragment-completed? 'anthill-grabberkin-fingers-inquiry)))
            )
           (make-decision
            #:title "Ask for more."
            #:description "\"EFFECT: 3 GRAMS OF GOLD.\", Otava retorts, citing increased future risks and recent local tensions as factors increasing not only production costs on part of supplier, but value to purchaser."
            #:next-fragment 'anthill-grabberkin-fingers-haggle
            )
           ))

(fragment 'anthill-grabberkin-fingers-haggle
          (thunk
           (p "There's a bustle of a/cogitation, as Anthill ponders the shifting of timelines and futures.")
           (define success?
             (attribute-check "Charisma" (actor-charisma (pc))))
           (cond [success?
                  (p "\"NEW CAUSES <–> NEW EFFECTS. adjustment procedure: EFFECT: 3 GRAMS.\", Anthill finally agrees.")
                  (add-new-task
                   (task
                    'grabberkin-fingers
                    "Grabberkin fingers for Anthill"
                    (partially-completed 0 5)
                    "Cause and effect: 5 fingers -> 3 g gold."
                    "In progress: 0"
                    '()
                    '()
                    '()
                    ))
                  ]
                 [else
                  (p "\"PREDICTION inconsistent-with-data / insightful. PREVIOUS CAUSE: UNCHANGED. PREVIOUS EFFECT: CHANGED.\", Anthill finally says after a long while. \"NEW EFFECT: 1 GRAM.\"")
                  (add-new-task
                   (task
                    'grabberkin-fingers
                    "Grabberkin fingers for Anthill"
                    (partially-completed 0 5)
                    "Cause and effect: 5 fingers -> 1 g gold."
                    "In progress: 0"
                    '()
                    '()
                    '()
                    ))
                  ])
           '()
           ))

(fragment 'anthill-grabberkin-fingers
          (thunk
           (create-task 'grabberkin-fingers)
           (wait-for-confirm)
           '()
           ))

(fragment 'anthill-grabberkin-inquiry
          (thunk
           (dev-note "xyzzy")
           (wait-for-confirm)
           '()
           )
          #:decisions
          (list
           (make-decision
            #:title "--"
            #:description "[Info goes here]"
            #:next-fragment 'anthill-1
            )))

(fragment 'anthill-grabberkin-fingers-inquiry
          (thunk
           (dev-note "bork")
           (wait-for-confirm)
           '()
           )
          #:decisions
          (list
           (make-decision
            #:title "--"
            #:description "[Info goes here]"
            #:next-fragment 'anthill-1
            )))

(fragment 'anthill-2
          (thunk
           (p "There's a hostile air around the anthill. Soldiers run into position. Anthill does not wish to communicate.")
           (p "Dismayed, Otava turns back.")

           (wait-for-confirm)
           (dev-note "TODO: advance-time-by-jiffies should be implemented as a field of fragment instead")
           (advance-time-by-jiffies! 10)))

(fragment 'anthill-complete-fingers
          (thunk
           (p "Looming anthill rises far far above Otava, blocking out much of the sky.")
           (wait-for-confirm)
           '()
           ))
