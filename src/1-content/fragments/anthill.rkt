#lang at-exp racket

(provide (all-defined-out))

(require "../../0-engine/0-api/api.rkt")

; destroy / prime / use / find / finish / fix / steal / steal plans to / prevent use of
(fragment 'anthill-teleporter
  (thunk
   (p @~a{
ANTHILL:
It destroys the Teleporter.

OTAVA:
Yes. Where is it?

ANTHILL:
Underneath, in the darkbelow, strongwalls, bigheavy surrounding. Coldneath watercurrent throughabove fallsound.

OTAVA:
Yes. How do I find it?

ANTHILL:
Twosplit vectorspace pathkind known. Riverway upwards bigwall consistency alteration structures. Birdwing humankind peakthrough drillsite tunnelbore splitwalk.
   })
   (add-new-task
    (task
    'destroy-the-teleporter
    "Destroy the Teleporter"
    "It is underneath, in Martaanvuo."
    "Not destroyed."
    '()
    '()
    '()
    ))))

(fragment 'anthill-1

          (thunk
           (p @~a{
 The anthill is huge. It is much taller than Otava, and it is bustling. Workers transporting raw materials, preparing, cutting, gluing, constructing. Others are tending to their crops and livestock. An orderly platoon of soldiers marches round the base. When Otava gets closer, structure emerges from the chaos of thousands of ants, and the anthill is talking to her.

 "BEGIN-COMMUNICATION. WHAT-IS: its type / identity?", the anthill asks.

 "human / it-calls-itself Otava the Seeker.", Otava responds.

 "CAUSE : Otava the Seeker BRINGS 2 GRABBERKIN FINGERS
 EFFECT: 2 GRAM OF GOLD
 AGREE?", the anthill asks.
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
            #:description "\"INQUIRY: grabberkin: description? DESCRIBE.\", Otava asks."
            #:next-fragment 'anthill-grabberkin-inquiry
            #:requirement (λ () (not (fragment-completed? 'anthill-grabberkin-inquiry)))
            )
           (make-decision
            #:title "Inquire about Grabberkin fingers."
            #:description "\"INQUIRY: grabberkin: finger: uses? DESCRIBE.\", Otava asks."
            #:next-fragment 'anthill-grabberkin-fingers-inquiry
            #:requirement (λ () (not (fragment-completed? 'anthill-grabberkin-fingers-inquiry)))
            )
           (make-decision
            #:title "Ask for more."
            #:description "\"EFFECT: 3 GRAMS OF GOLD.\", Otava retorts, citing increased future risks and recent local tensions as factors increasing not only production costs on part of supplier, but value to purchaser, trying to circumvent her opponent's logic."
            #:next-fragment 'anthill-grabberkin-fingers-haggle
            )
           (make-decision
            #:title "Disagree."
            #:description "\"NO DEAL.\", Otava says and leaves."
            #:next-fragment 'exit
            )
           ))

(fragment 'anthill-1-b

          (thunk
           (p @~a{
 The anthill waits for Otava to respond to the postulated causal chain of the cause of bringing a pair of grabberkin fingers leading to the effect of 2 grams of gold.
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
            #:description "\"INQUIRY: grabberkin: description? DESCRIBE.\", Otava asks."
            #:next-fragment 'anthill-grabberkin-inquiry
            #:requirement (λ () (not (fragment-completed? 'anthill-grabberkin-inquiry)))
            )
           (make-decision
            #:title "Inquire about Grabberkin fingers."
            #:description "\"INQUIRY: grabberkin: finger: uses? DESCRIBE.\", Otava asks."
            #:next-fragment 'anthill-grabberkin-fingers-inquiry
            #:requirement (λ () (not (fragment-completed? 'anthill-grabberkin-fingers-inquiry)))
            )
           (make-decision
            #:title "Ask for more."
            #:description "\"EFFECT: 3 GRAMS OF GOLD.\", Otava retorts, citing increased future risks and recent local tensions as factors increasing not only production costs on part of supplier, but value to purchaser, trying to circumvent her opponent's logic."
            #:next-fragment 'anthill-grabberkin-fingers-haggle
            )
           (make-decision
            #:title "Disagree."
            #:description "\"NO DEAL.\", Otava says and leaves."
            #:next-fragment 'exit
            )
           ))

(fragment 'anthill-grabberkin-fingers-haggle
          (thunk
           (p "Bustling of agitation and cogitation happens as Anthill ponders shifting timelines and futures.")
           (define success?
             (attribute-check "Charisma" (actor-charisma (pc))))
           (cond [success?
                  (p "\"NEW CAUSES <–> NEW EFFECTS. EFFECT: 3 GRAMS.\", Anthill finally agrees.")
                  (add-new-task
                   (task
                    'grabberkin-fingers
                    "Grabberkin fingers for Anthill"
                    (partially-completed 0 2)
                    "Cause and effect: 2 fingers -> 3 g gold."
                    "In progress: 0"
                    '()
                    '()
                    '()
                    ))
                  ]
                 [else
                  (p "\"PREDICTION inconsistent-with-data / insightful. PREVIOUS CAUSE: UNCHANGED. PREVIOUS EFFECT: CHANGED.\", Anthill finally says after a long while.")
                  (wait-for-confirm)
                  (p "\"CAUSE: 2 GRABBERKIN FINGERS. EFFECT: 1 GRAM GOLD.\"")
                  (add-new-task
                   (task
                    'grabberkin-fingers
                    "Grabberkin fingers for Anthill"
                    (partially-completed 0 5)
                    "Cause and effect: 2 fingers -> 1 g gold."
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
           (add-new-task
            (task
             'grabberkin-fingers
             "Grabberkin fingers for Anthill"
             (partially-completed 0 2)
             "Cause and effect: 2 fingers -> 2 g gold."
             "In progress: 0"
             '()
             '()
             '()
             ))
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
            #:next-fragment 'anthill-1-b
            )))

(fragment 'anthill-grabberkin-fingers-inquiry
          (thunk
           (p "\"Marchgates\", Anthill begins, \"tear through space-time.\" Two trees near Otava turn into huge, mangled fingers, bloody and tortured. The bones of the fingers twist and snap, as the fingers contort into two angular, asymmetric arches leaning onto one another. The arches converge, and there's the heavy white indoor walls of a prison-like facility, blood splattered on the walls and a pile of carnage and limbs and mutilated bodies. There's a pile of tongues of different sizes, dripping blood.")
           (p "A glint of gold from the broken teeth of a gnashed mouth of a crushed face of a man. He's around 35, stubble and long dark hair. His head has been smashed in from the right, a bloody pulpy mess where there used to be the other half of his face.")
           )
          #:decisions
          (list
           (make-decision
            #:title "--"
            #:description "The marchgates tower over Otava, dripping warm blood, crushed-finger-scaffolding holding up the world. She blinks and the vision is gone and she's back at the anthill."
            #:next-fragment 'anthill-1-b
            )))

(fragment 'anthill-2
          (thunk
           (p "There's a hostile air around the anthill. Soldiers run into position. Anthill does not wish to communicate.")
           (p "Dismayed, Otava turns back.")

           (wait-for-confirm)
           '()
           )
          #:time-taken-by-fragment 50
          )

(fragment 'anthill-complete-fingers
          (thunk
           (p "Looming anthill rises far far above Otava, blocking out much of the sky.")
           (wait-for-confirm)
           '()
           ))
