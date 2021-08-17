#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "fragments/anthill.rkt")

(require "action.rkt")
(require "checks.rkt")
(require "decision.rkt")
(require "location.rkt")
(require "fragment.rkt")
(require "io.rkt")
(require "item.rkt")
(require "pc.rkt")
(require "quest.rkt")
(require "quests.rkt")
(require "situation.rkt")
(require "utils.rkt")
(require "world.rkt")

(lazy-require
 ["round-resolver.rkt"
  (resolve-pc-action!)])


; This should happen on the beginning of a life
; and with runs, you select the loadout
(fragment
 1
 (proc
  (paragraph
   "Still, she has reason for confidence."
   ))
 #:decisions
 (list
  (make-decision
   #:title "She has a gun."
   #:description "Her Aegis Metalworks revolver will get her out of a pinch. One of the last ones they ever made before the Rains. Hundreds of years old, and more reliable than anything that's made since."
   #:on-resolve! (proc
                  (set-build! 'gun)
                  (wait-for-confirm)
                  (paragraph "The trail goes past some jagged pieces of metal that stand up from the ground. This is anomaly perimeter, then – she's getting close.")
                  (paragraph "The trail turns behind a boulder and comes to a fork. A fork? Broker had said nothing about a fork.")
                  (paragraph "The left branch turns into a climb up a rocky hill. A magpie's call echoes from somewhere up the hill. An army of ants is marching down the other branch, toward what must be Martaanvuo swamp."))
   #:next-fragment 'exit
   )

  (make-decision
   #:title "She punches really hard."
   #:description "She can crack a jawbone with her bare hands. The trick is in hitting from the side and from below."
   #:on-resolve! (proc
                  (set-build! 'bruiser)
                  (wait-for-confirm)
                  (paragraph "The trail goes past some jagged pieces of metal that stand up from the ground. This is anomaly perimeter, then – she's getting close.")
                  (paragraph "The trail turns behind a boulder and comes to a fork. A fork? Broker had said nothing about a fork.")
                  (paragraph "The left branch turns into a climb up a rocky hill. A magpie's call echoes from somewhere up the hill. An army of ants is marching down the other branch, toward what must be Martaanvuo swamp."))
   #:next-fragment 'exit)

  (make-decision
   #:title "She's a survivor."
   #:description "She is a survivor. That's what's kept her alive all these years. That, and her modified bolt cutters."
   #:on-resolve! (proc
                  (set-build! 'survivor)
                  (wait-for-confirm)
                  (paragraph "The trail goes past some jagged pieces of metal that stand up from the ground. This is anomaly perimeter, then – she's getting close.")
                  (paragraph "The trail turns behind a boulder and comes to a fork. A fork? Broker had said nothing about a fork.")
                  (paragraph "The left branch turns into a climb up a rocky hill. A magpie's call echoes from somewhere up the hill. An army of ants is marching down the other branch, toward what must be Martaanvuo swamp."))
   #:next-fragment 'exit)
  )
 )







(fragment
 20
 (proc
  (paragraph
   "Otava thinks the magpie should be close, but the sound seems to come from a slightly different direction every time."
   ))
 
 #:decisions
 (list (make-decision
        #:title "Listen quietly."
        #:description "The magpie laughs straight above her. The bird is hidden somewhere within the shadowy branches of a large dead oak. There's a worn tombstone half buried under the roots of the tree."
        #:on-resolve! (proc
                       (remove-feature-from-location! (current-location) 'magpie-effigy))
        #:next-fragment 'exit)
       (make-decision
        #:title "Listen quietly, with a gun in the hand."
        ; TODO this should be The Eternal Bullet, which would have a chance of not being consumed, and a chance of seriously messing with the ontology of the world
        #:description "There's a flutter. The outline of a tree shakes as the magpie takes flight and disappears, unseen. Otava jogs the rest of the way. At the bottom of the tree there are the decaying remains of a frog. Ants have created a temporary bypass around the rotting carcass. Something glimmery catches Otava's eyes."
        #:on-resolve! (proc
                       (paragraph "Otava picks up a revolver cartridge. Ordinary-looking, and the right size.")
                       (set-flag 'eternal-bullet)
                       (add-ammo! 1)
                       (remove-feature-from-location! (current-location) 'magpie-effigy))
        #:next-fragment 'exit)))




(fragment
 100
 (proc
  (paragraph
   "[post-combat steps to do]"
   ))
 
 #:decisions
 (list (make-decision
        #:title "Exit action."
        #:description "Combat finished."
        #:next-fragment 'exit
        )))

#;(fragment
   200
   (proc
    (paragraph
     "[post-combat steps to do]"
     ))
   (string-append
    "A complicated tangle of wires and pipes fill much of the back half of the room. On a cluttered desk to the side there's a pile of schematics and notes."
    "\n\n"
    "Hartmann Device mk. II."
    "\n\n"
    "The sequence to power on the device is described on a series of handwritten notes scribbled in the margin of one of the myriad of the schematics."
    )
   #:decisions
   (list
    (make-decision
     #:title "Power on the device."
     #:description "Otava dives through the jungle of cables and pipes, connecting what needs to be connected, turning on what needs to be turned on. After an hour of work, the device finally comes to life. Otava flicks the switch to begin the process, and a temperature gauge starts plummeting.\n\nAs soon as the temperature inside the kernel chamber of the device reaches point-triple-zero-one Kelvin, the zero-point field within falls to a lower state of energy, commencing a chain reaction proceeding at the speed of light from the kernel outwards. As the substratum of physical existence unfolds, matter and energy and time and space irreversibly cease to exist. Otava blinks out of existence along with the rest of the universe, never to be born again."
     #:next-fragment 'exit
     #:on-resolve! (λ () (end-game))
     )

    (make-decision
     #:title "Leave the device be."
     #:description "Otava leaves Hartmann Device mk. II alone, wondering what might have been."
     #:next-fragment 'exit
     )))



(fragment
 'magpie
 (proc
  (paragraph
   "The young magpie is perched in a tree, a gray-and-black silhoutte amidst a gray-and-black forest of silhouettes, trying to keep dry. \"Hello, human\", it says, \"I am... Magpie. I am King of Magpies, Guardian of Forest, and I need help. After the skies bled, a Menace rose from the Facility, and we are the only thing between it and the World. Yet we are few, and we are hungry and weary. Can you help us?\""
   ))
 
 #:decisions
 (list
  (make-decision
   #:title "Donate a lot."
   #:description "\"Hello, Magpie, King of Magpies, Guardian of Forest, I am Otava. I don't have much, but you can have this.\""
   #:next-fragment 'magpie-donate-a-lot
   )
  (make-decision
   #:title "Donate a bit."
   #:description "\"Hello, Magpie, King of Magpies, Guardian of Forest, I am Otava. I don't have much, but this I can spare.\""
   #:next-fragment 'magpie-donate-a-bit
   )
  (make-decision
   #:title "Do not donate."
   #:description "\"Hello, Magpie, King of Magpies, Guardian of Forest, I am Otava. I am sorry, but I don't have much.\""
   #:next-fragment 'magpie-do-not-donate
   )
  ))

(fragment
 'magpie-donate-a-lot
 (proc
  (paragraph
   "Otava offers her revolver. \"Thank you\", Magpie says, \"with this I can shoot my enemies. But how will you shoot yours? I hereby dub thee Otava the Altruist.\""
   )))

(fragment
 'magpie-donate-a-bit
 (proc
  (paragraph
   "Otava offers two of her bullets. \"Thank you\", Magpie says, \"with these I can shoot my enemies. I hereby dub thee Otava the Friend.\""
   )))

(fragment
 'magpie-do-not-donate
 (proc
  (paragraph
   "\"I hope future brings you good fortune\", Magpie says."
   )))



(fragment
 'begin-stiltman-dialogue
 (proc
  (next-chapter!)
  (paragraph "\"– ah, I knew you would ask that – no, I insist – you can call me Stiltman – I am unstably present –\", the man exclaims, stumbling and wobbling in the cove, when he notices Otava approaching. Stiltman is wearing the overalls of a lab technician, and is strapped from his waist to the pipes and rods of his three-legged contraption. The logo on the overalls says Murkwater–Aegis, and there's a name tag saying STILTMAN.")
  (remove-feature-from-location! (current-location) 'stiltman)
  (paragraph "\"What -\" Otava begins, but Stiltman goes on. \"– the Anthead Monograph, it's an ancient god, there's a lot of metaphysical woo-woo but the logic system is interesting – here's the fee –\"")
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
    (paragraph "\"– It is a completely different basis for mathematics, see, so then new foundations lead to new mechanisms which then led them to a new kind of physics –\"")
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
    (paragraph "\"– we all got stuck, see – the Murkwater-Aegis facility upriver – the Monograph contains the solution – \", Stiltman goes, \"– in the storage closet of the workshop – it is locked – no, not the closet, the book – it was an accident – nobody asked where did it come from –\"")
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
    (paragraph "\"– who would have thought – the Murkwater-Aegis facility upriver – the book must be destroyed, the area sealed, the public will have to be informed – Uncolor is looking for it and she must not get it – \", Stiltman goes, \"– I hid the Monograph in the storage closet of the workshop – front payment, kilos more –\" It looks like Stiltman is pulled back by invisible ropes. \"– she went insane from reading it, rambling about the Anthead  – do not read it! –\", he shouts, disappearing in the mist")

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


(fragment
 'turn-on-martaanvuo-terminal
 (proc
  (paragraph
   "The password is written down on a piece of paper. The terminal has access to heating and ventilation systems, including service access to reactor. There is also a lone executable with the name 'Martaanvuo'."))
 #:decisions
 (list
  (make-decision
   #:title "HVAC and service access to reactor."
   #:description "Otava takes a glance at the familiar-looking systems."
   #:next-fragment 'exit
   )

  (make-decision
   #:title "Launch Martaanvuo."
   #:description "The terminal greets her:"
   #:next-fragment 'recurse
   )))

