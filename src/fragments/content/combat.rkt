#lang racket

(provide (all-defined-out))

(require "../../core/api.rkt")


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

