#lang racket

(provide (all-defined-out))

(require "../core/api.rkt")


(fragment
 'begin-life
 (thunk
  (p
   "She goes over her checklist again: Plan? Part of a plan, yes. Knife? Yes. Provisions? For a couple of days, yes. Bolt cutters? Yes."
   )
  )
 #:decisions
 (list
  (make-decision
   #:title "Rope."
   #:description "Rope, about 40 meters, mostly not too badly frayed."
   #:on-resolve! (thunk
                  (set-build! 'rope)
                  (wait-for-confirm)

                  (current-show-round-summary? #t))
   #:next-fragment 'begin-life-exit
   )

  (make-decision
   #:title "Flashlight."
   #:description "A flashlight, with almost half of a full charge."
   #:on-resolve! (thunk
                  (set-build! 'flashlight)
                  (wait-for-confirm)

                  (current-show-round-summary? #t))
   #:next-fragment 'begin-life-exit)
  )
 )

(fragment
'begin-life-exit
(thunk
  (p "The trail goes past some jagged pieces of metal that stand up from the ground. A half-buried sensor array, long out of commission. This is anomaly perimeter, then - she's getting close. No rust after decades, damn good metal. Maybe when she's coming back try to cut some, take it with her. Could be useful maybe.")
  (p "Otava comes to a fork.")
  (opts
    "The Broker..." "The Broker had said nothing about a fucking fork. Sounds like a fucking refund."
    "The fork..." "The fork is an unwelcome surprise, the damp air a chilly suffocation against her skin.")
  (p "The left branch turns into a climb up a rocky hill. A magpie's call echoes from somewhere up the hill. An army of ants is marching down the other branch, toward what must be Martaanvuo swamp.")
  ))
