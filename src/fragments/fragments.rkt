#lang racket

(provide (all-defined-out))


(require "../core/api.rkt")

(require
  "anthill.rkt"
  "decision.rkt"
  "fragment.rkt"
  "magpie.rkt"
  "stiltman.rkt")

(require "../state/state.rkt")

; This should happen on the beginning of a life
; and with runs, you select the loadout
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
  (p "The trail goes past some jagged pieces of metal that stand up from the ground. This is anomaly perimeter, then – she's getting close.")
  (p "The trail turns behind a boulder and comes to a fork. A fork? Broker had said nothing about a fork.")
  (p "The left branch turns into a climb up a rocky hill. A magpie's call echoes from somewhere up the hill. An army of ants is marching down the other branch, toward what must be Martaanvuo swamp.")))




(fragment
 100
 (thunk
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
 (thunk
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
 (thunk
  (p "Otava tumbles down along the near-vertical wall, tries to get a grip and slams headfirst against a granite fist. She feels weightless as the world spins around her. A rock pillar extends towards her and there's a crunch as her hand gets caught in a crack and her body wraps around the pillar.")
  (wait-for-confirm)
  (p "Warm red black darkness tastes like iron.")
  (wait-for-confirm)
  ;; Here: Possible flashback aka new life
  (p "Consciousness fades into the void.")
  (wait-for-confirm)
  #;(define r (d 1 4))
  (define r 1)
  (define success-text
    (if (= r 1)
        "failure"
        "success"))
  (info-card
   (tbody
    (tr " 1d4: "
        (format " ~a [~a] " r success-text)))
   "Death save")
  (wait-for-confirm)
  (case r
    [(1)
     (kill-pc! 'fell-to-death)
     'pc-dead]
    [(2 3 4)
     (next-chapter!)
     (p "Otava comes to.")
     (wait-for-confirm)
     (p "Then, the pain.")
     (wait-for-confirm)])
 )
 #:decisions
 (list (make-decision
        #:title "..."
        #:next-fragment 'fall-down-2
        #:requirement (λ () (pc-is-alive?))
        )
        (make-decision
        #:title "DEAD"
        #:next-fragment 'pc-dead
        #:requirement (λ () (not (pc-is-alive?)))
        )))

(fragment
 'fall-down-2
 (thunk
  (p "..."))
 #:decisions
 (list (make-decision
        #:title "..."
        #:next-fragment 'exit
        )))
