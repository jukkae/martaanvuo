#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "api.rkt")

(require "fragments/anthill.rkt"
         "fragments/magpie.rkt"
         "fragments/stiltman.rkt")

(require "action.rkt")
(require "checks.rkt")
(require "decision.rkt")
(require "fragment.rkt")
(require "situation.rkt")
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

