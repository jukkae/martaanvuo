#lang racket

(provide (all-defined-out))

(require "../../core/api.rkt")


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
