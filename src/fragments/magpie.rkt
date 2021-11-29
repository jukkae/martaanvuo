#lang racket

(provide (all-defined-out))

(require "../core/api.rkt")

(fragment
 'magpie
 (thunk
  (p
   "The young magpie is perched in a tree, a gray-and-black silhoutte amidst a gray-and-black forest of silhouettes, trying to keep dry. \"Good day, human\", it says, \"I am... Magpie. I am King of Magpies, Guardian of Forest. Are you looking for work?\"")
  )

 #:decisions
 (list
  (make-decision
   #:title "Yes."
   #:description "\"Good day, Magpie, King of Magpies. I am Otava, and I seek gold. Tell me more.\""
   #:next-fragment 'magpie-cache
   )))

(fragment
 'magpie-cache
 (thunk
  (p
   "Magpie ruffles its feathers, as it asks you to come closer and leans in. In low voice that is all but lost under the falling rain, it begins: \"There's a cache nearby. I need someone with fingers to take care of various locks and other systems. In and out, we'll be done before the day is done. It looks like it's completely abandoned. I can assure you, it's absolutely almost risk-free. The pay is 23 gold coins. What do you say?\"")
  )

 #:decisions
 (list
  (make-decision
   #:title "23 is an interesting number."
   #:next-fragment 'magpie-interesting
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
   "Otava agrees, and Magpie goes over the details. The cache is in the nearby power plant on the plateau. It should be possible to get enough power to open the doors, and if not, well, they're sure to come up with something.")
  (create-quest 'loot-the-cache))

  #:decisions
 (list
  (make-decision
   #:title "Ask about the benefits package."
   #:description "Magpie explains the benefits package and armory credit system in detail, but it is all terribly confusing. In the end, Otava nevertheless gets a revolver."
   #:on-resolve! (thunk
                  (add-item! 'revolver)
                  (wait-for-confirm))
   #:next-fragment 'exit
   )
  ))
