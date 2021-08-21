#lang racket

(provide (all-defined-out))

(require "../api.rkt")

(fragment
 'magpie
 (proc
  (p
   "The young magpie is perched in a tree, a gray-and-black silhoutte amidst a gray-and-black forest of silhouettes, trying to keep dry. \"Hello, human\", it says, \"I am... Magpie. I am King of Magpies, Guardian of Forest. Who are you and what is your business?\"")
  )
 
 #:decisions
 (list
  (make-decision
   #:title "Otava, seeking gold."
   #:description "\"I am Otava, and I am seeking gold.\""
   #:next-fragment 'magpie-help
   )))

(fragment
 'magpie-help
 (proc
  (p
   "\"Chk-chk! Gold! I happen to know quite a bit about gold! In fact, I will tell you about a vault somewhere in the ruins nearby, if we agree on a deal, chk-chk-chk!\"")
  (p
   "\"What's the catch?\", Otava asks.")
  (p
   "\"No catch, some hundreds of grams at least, we split 30-70, I just need hands to break in\", Magpie explains.")
  (p
   "\"And the risks?\", Otava asks.")
  (p
   "\"As long as you don't go into the tunnels or do anything stupid, the worst might be some local wildlife, chk!\", Magpie says. \"So, what do you say?\"")
  )
 
 #:decisions
 (list
  (make-decision
   #:title "Agree."
   #:description "\"Sounds good, tell me more.\""
   #:next-fragment 'magpie-agree
   )
  ))

(fragment
 'magpie-agree
 (proc
  (p "\"So, in the power plant a bit ahead on this plateau, there's one of the storage closet's been turned into a vault. Looks like been people camping there, possibly raiders, possibly looters – when it happened, some saw, uh, opportunity.\", Magpie explains."
   )
  (p "\"When what happened?\", Otava asks."
   )
  )
 
 #:decisions
 (list
  (make-decision
   #:title "Agree."
   #:description "\"Sounds good, tell me more.\""
   #:next-fragment 'exit ; actually, why not just treat '() as exit...
   )
  ))

(fragment
 'magpie-donate-a-lot
 (proc
  (p
   "Otava offers her revolver. \"Thank you\", Magpie says, \"with this I can shoot my enemies. But how will you shoot yours? I hereby name you Otava the Friend.\""
   )))

(fragment
 'magpie-donate-a-bit
 (proc
  (p
   "Otava offers two of her bullets. \"Thank you\", Magpie says, \"with these I can shoot my enemies.\""
   )))

(fragment
 'magpie-do-not-donate
 (proc
  (p
   "\"I hope future brings you good fortune\", Magpie says."
   )))