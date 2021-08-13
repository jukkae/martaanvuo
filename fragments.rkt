#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "action.rkt")
(require "checks.rkt")
(require "decision.rkt")
(require "location.rkt")
(require "fragment.rkt")
(require "io.rkt")
(require "item.rkt")
(require "pc.rkt")
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
 "Still, someone's been there, and so will she. And she'll make it back, too."
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
   #:description "She is a survivor. That's what's kept her alive all these years."
   #:on-resolve! (proc
                  (set-build! 'survivor)
                  (wait-for-confirm)
                  (paragraph "The trail goes past some jagged pieces of metal that stand up from the ground. This is anomaly perimeter, then – she's getting close.")
                  (paragraph "The trail turns behind a boulder and comes to a fork. A fork? Broker had said nothing about a fork.")
                  (paragraph "The left branch turns into a climb up a rocky hill. A magpie's call echoes from somewhere up the hill. An army of ants is marching down the other branch, toward what must be Martaanvuo swamp."))
   #:next-fragment 'exit)
  )
 #:on-enter!
 (λ () '()#;(set-prompt! "Because...")))







(fragment
 20
 (string-append
  "Otava thinks the magpie should be close, but the sound seems to come from a slightly different direction every time."
  )
 
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
 (string-append
  "[post-combat steps to do]"
  )
 
 #:decisions
 (list (make-decision
        #:title "Exit action."
        #:description "Combat finished."
        #:next-fragment 'exit
        )))

(fragment
 200
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
 'begin-stiltman-dialogue
 "Stiltman goes quiet and seems to struggle against an unseen wind."

 #:on-enter!
 (proc
  (next-chapter!)
  (paragraph "\"– ah, I knew you would ask that – no, I insist – you can call me Stiltman – I am unstably present –\", the man exclaims, stumbling and wobbling in the mire, when he notices Otava approaching. Stiltman is wearing the overalls of a lab technician, and is strapped from his waist to the pipes and rods of his three-legged contraption. The logo on the overalls says Murkwater–Aegis.")
  (remove-feature-from-location! (current-location) 'stiltman)
  (paragraph "\"What -\" Otava begins, but Stiltman goes on. \"– the Anthead Monograph, it's an ancient god, there's a lot of metaphysical woo-woo but the logic system is interesting – here's the fee –\"")
  (paragraph "Stiltman throws something on the pier, and it lands with the metal clink of coins. Otava kneels to open the bag, while watching Stiltman. There's a handful of small gold coins in the bag.")
  (add-item! 'gold #:amount 11 #:title "Picked up")
  (create-quest 'anthead-monograph)
  (set-flag 'ending-run-allowed))
 
 #:decisions
 (list

  ; -> there is no mission, but something else happens
  (make-decision
   #:title "Ask what he means, 'unstably present'."
   #:on-resolve!
   (proc
    (paragraph "\"What do you mean, 'unstably present'?\", Otava asks.")
    (paragraph "\"– I was fishing, thank the heavens you noticed me – \", Stiltman goes, \"– they were manufacturing a special kind of radioactive gold, for medicine and things – I'm just a technician, see – \"")
    (paragraph "\"– It is a completely different basis for mathematics, see, so then new foundations lead to new mechanisms which then led them to a new kind of physics –\"")
    (paragraph "\"– so then we ran the simulations, and everything checked out and we were well within the parameter range even in the worst case – so then we could, well, create gold out of thin air –\", Stiltman says, as he's beginning to slip away, like gravity is suddenly sideways for him. \"– so then we modified the reactor and – I've been here for weeks – different causes lead to different effects –\", he stutters, as he disappears in the mist.")
    )
   #:next-fragment 'exit
   )
  ; -> the mission is to find the monograph
  (make-decision
   #:title "Ask about the Monograph."
   #:description "\"The Monograph?\", Otava asks."
   #:next-fragment 'exit
   )
  ; -> the mission is to destroy the monograph
  (make-decision
   #:title "Ask about what the fee is for."
   #:on-resolve!
   (proc
    (paragraph "\"The fee, what is it for?\", Otava asks.")
    (paragraph "\"– who would have thought – the Murkwater-Aegis facility upriver – the book must be destroyed, the area sealed, the public will have to be informed – Uncolor is looking for it and she must not get it – \", Stiltman goes, \"– I hid the Monograph in the storage closet of the workshop – front payment, kilos more –\" It looks like Stiltman is pulled back by invisible ropes. \"– watch out for the Magpie! –\", he shouts, disappearing in the mist")
    )
   #:next-fragment 'exit
   )))



