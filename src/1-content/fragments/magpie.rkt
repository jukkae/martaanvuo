#lang at-exp racket

(provide (all-defined-out))

(require "../../0-engine/0-api/api.rkt")

(fragment
 'magpie
 (thunk
  (p
   "The young magpie is perched in a tree, a gray-and-black silhoutte amidst a gray-and-black forest of silhouettes. \"Good day, human\", it says, \"I am... Magpie. I am King of Magpies, Guardian of Forest. State your affairs here.\"")
  )

 #:decisions
 (list
  (make-decision
   #:title "\"I am Otava, the Seeker, and I seek gold.\""
   #:description "\"Good day, Magpie, King of Magpies. I am Otava the Seeker, and I seek gold.\""
   #:next-fragment 'magpie-cache
   )
  ))

(fragment
 'magpie-cache
 (thunk
  (p
   "\"You're in luck\", Magpie says. It ruffles its feathers, as it asks you to come closer and leans in. In low voice, it begins: \"There's a cache nearby. I need someone with fingers to take care of various locks and other systems. In and out, we'll be done before the day is done. It looks like it's completely abandoned. I can assure you, it's absolutely almost risk-free. The pay is 23 silver coins. What do you say?\"")
  )

 #:decisions
 (list
  (make-decision
   #:title "\"23 is an interesting number.\""
   #:next-fragment 'magpie-interesting
   )
  (make-decision
   #:title "\"I'm looking for gold.\""
   #:next-fragment 'magpie-gold
   )
  ))

(fragment
 'magpie-interesting
 (thunk
  (p
   "\"It's, uh... 23 is an interesting number. Considering the requirements and the expertise required, I was thinking more along the lines of –\", Otava says, only briefly pausing while she does the math, \"– 37.\"")
  (p
   "\"31 and perks\", Magpie replies, \"I'll explain the whole package in detail later, but it includes things like tailored excellent employee recognition messages, free armory credits, and fitness and wellness perks.\"")
  (p
   "Otava agrees, and Magpie goes over the details. The cache is in the nearby scientific outpost on the plateau.")
  )

  #:decisions
 (list
  (make-decision
   #:title "Ask about the benefits package."
   #:description "Magpie explains the benefits package and armory credit system in detail, but it is all terribly confusing. In the end, Otava nevertheless gets a revolver."
   #:on-resolve! (thunk
                  (add-item! 'revolver)
                  (wait-for-confirm)
                  '())
   #:next-fragment 'exit
   )
  ))

(fragment
 'magpie-gold
 (thunk
  (p
   "\"I'm looking for gold\", Otava says.")
  (p
   "\"I know, but I have none, you can exchange the silver, it's as good as gold, only cheaper\", Magpie says. \"So, what do you say, 23 silver coins? Would amount to, uh, 0.23 gold coins, which is, you know, a couple of grams.\""))
 #:decisions
 (list
  (make-decision
   #:title "\"23 is an interesting number.\""
   #:next-fragment 'magpie-interesting
   )))