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
(require "state/state.rkt")
(require "world.rkt")

; This should happen on the beginning of a life
; and with runs, you select the loadout
(fragment
 'begin-life
 (proc
  (p
   (string-append
    "She goes over her checklist again: Plan? Part of a plan, yes. Map? Not applicable. Knife? Yes. Food? Uh... not much. Bolt cutters? Yes. Bonding tape, firestarter, bivouac. Something else?")
   )
  )
 #:decisions
 (list
  (make-decision
   #:title "Rope."
   #:description "Rope, about 20 meters, mostly not too badly frayed."
   #:on-resolve! (proc
                  (set-build! 'survivor)
                  (wait-for-confirm)
                  (p "The trail goes past some jagged pieces of metal that stand up from the ground. This is anomaly perimeter, then – she's getting close.")
                  (p "The trail turns behind a boulder and comes to a fork. A fork? Broker had said nothing about a fork.")
                  (p "The left branch turns into a climb up a rocky hill. A magpie's call echoes from somewhere up the hill. An army of ants is marching down the other branch, toward what must be Martaanvuo swamp."))
   #:next-fragment 'exit
   )

  (make-decision
   #:title "A flashlight."
   #:description "A flashlight."
   #:on-resolve! (proc
                  (set-build! 'survivor)
                  (wait-for-confirm)
                  (p "The trail goes past some jagged pieces of metal that stand up from the ground. This is anomaly perimeter, then – she's getting close.")
                  (p "The trail turns behind a boulder and comes to a fork. A fork? Broker had said nothing about a fork.")
                  (p "The left branch turns into a climb up a rocky hill. A magpie's call echoes from somewhere up the hill. An army of ants is marching down the other branch, toward what must be Martaanvuo swamp."))
   #:next-fragment 'exit)
  )
 )




(fragment
 100
 (proc
  (p
   "[post-combat steps]"
   ))
 
 #:decisions
 (list (make-decision
        #:title "Exit combat."
        #:description "Combat finished."
        #:next-fragment 'exit
        )))




(fragment
 'turn-on-martaanvuo-terminal
 (proc
  (p
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

(fragment
 'fall-down
 (proc
  (dev-note "oof")
  (p "Otava's grasp slips. She tries to find a footing."))
 #:decisions
 (list (make-decision
        #:title "ok"
        #:description "ok"
        #:next-fragment 'exit
        )))

