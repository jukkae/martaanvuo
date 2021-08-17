#lang racket

(provide (all-defined-out))

(require "../api.rkt")

(fragment
 'begin-stiltman-dialogue
 (proc
  (next-chapter!)
  (paragraph "\"– ah, I knew you would ask that – no, I insist – you can call me Stiltman – I am unstably present –\", the man exclaims, stumbling and wobbling in the cove, when he notices Otava approaching. Stiltman is wearing the overalls of a lab technician, and is strapped from his waist to the pipes and rods of his three-legged contraption. The logo on the overalls says Murkwater–Aegis, and there's a name tag saying STILTMAN.")
  (remove-feature-from-location! (current-location) 'stiltman)
  (paragraph "\"What -\" Otava begins, but Stiltman goes on. \"– the Anthead Monograph, it's an ancient god, there's a lot of metaphysical woo-woo but the logic system is interesting – here's the fee – it is like a Ophiocordyceps fungus, you know, the mind control –\"")
  (paragraph "Stiltman throws something on the pier, and it lands with the metal clink of coins. Otava kneels to open the bag, while watching Stiltman. There's a handful of small gold coins in the bag.")
  (add-item! 'gold #:amount 11 #:title "Picked up")
  (create-quest 'anthead-monograph)
  (set-flag 'ending-run-allowed)
  (paragraph
   "Stiltman abruptly quits talking."
   ))
 
 #:decisions
 (list

  ; -> there is no mission, but something else happens
  (make-decision
   #:title "Ask what he means, 'unstably present'."
   #:on-resolve!
   (proc
    (paragraph "\"What do you mean, 'unstably present'?\", Otava asks.")
    (paragraph "\"– I was fishing, thank the heavens you noticed me – \", Stiltman goes, \"– they were manufacturing a special kind of radioactive gold, for medicine and things – I'm just a technician, see, I was doing the night shift when it –\"")
    (paragraph "\"– It is a completely different basis for mathematics, see, so then new foundations lead to new mechanisms which then led them to a new kind of physics – no, I don't know what the biologists were doing! –\"")
    (paragraph "\"– so then we ran the simulations, and everything checked out and we were well within the parameter range even in the worst case – so then we could, well, create gold out of thin air –\", Stiltman says, as he's beginning to slip away, like gravity is suddenly sideways for him. \"– so then we modified the reactor and – I've been here for weeks – different causes lead to different effects – I need the book to get back! –\", he stutters, as he disappears in the mist.")

    (update-quest-status! 'anthead-monograph "find it")
    (update-quest-notes! 'anthead-monograph "-> Stiltman (lab tech); new kind of physics - gold!")
    (update-quest-details! 'anthead-monograph '(find-it))

    (define body
      (format-quest-for-card (quest-exists? 'anthead-monograph)))
    (info-card
     (list body)
     "Quest updated")

    (wait-for-confirm)
    )
   #:next-fragment 'exit
   )
  ; -> the mission is to find the monograph
  (make-decision
   #:title "Ask about the Monograph."
   #:on-resolve!
   (proc
    (paragraph "\"The Monograph?\", Otava asks.")
    (paragraph "\"– we all got stuck, see – for all I know they are the ones to blame, and Principal foremost – sorry, that'd be Mx Principal Scientist, affectionately known to her closest ones as... Mx Principal Scientist – the Murkwater-Aegis facility upriver – the Monograph contains the solution – \", Stiltman goes, \"– in the storage closet of the workshop – it is locked – no, not the closet, the book – it was an accident – nobody asked where did it come from –\"")
    (paragraph "Suddenly, Stiltman snaps back into the middle of the cove, and then further out near the cliffs, before disappearing. \"– I need the book! – gold at the facility – \" is the last Otava hears of Stiltman.")

    (update-quest-status! 'anthead-monograph "find it")
    (update-quest-notes! 'anthead-monograph "-> Stiltman; Murkwater-Aegis facility upriver - gold!")
    (update-quest-details! 'anthead-monograph '(find-it))

    (define body
      (format-quest-for-card (quest-exists? 'anthead-monograph)))
    (info-card
     (list body)
     "Quest updated")

    (wait-for-confirm)
    )
   
   #:next-fragment 'exit
   )
  ; -> the mission is to destroy the monograph
  (make-decision
   #:title "Ask about what the fee is for."
   #:on-resolve!
   (proc
    (paragraph "\"The fee, what is it for?\", Otava asks.")
    (paragraph "\"– who would have thought – the Murkwater-Aegis facility upriver – the book must be destroyed, the area sealed, the public will have to be informed – \", Stiltman goes, \"– I hid the Monograph in the storage closet of the workshop – front payment, kilos more –\" It looks like Stiltman is pulled back by invisible ropes. \"– they went insane from reading it, rambling about the Anthead  – do not read it! –\", he shouts, disappearing in the mist")

    (update-quest-status! 'anthead-monograph "destroy")
    (update-quest-notes! 'anthead-monograph "-> Stiltman; Murkwater-Aegis facility upriver - gold!")
    (update-quest-details! 'anthead-monograph '(destroy-it))

    (define body
      (format-quest-for-card (quest-exists? 'anthead-monograph)))
    (info-card
     (list body)
     "Quest updated")

    (wait-for-confirm)
    )
   #:next-fragment 'exit
   )))