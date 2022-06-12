#lang at-exp racket

(provide (all-defined-out))

(require "../../0-engine/0-api/api.rkt")


(fragment
 'turn-on-martaanvuo-terminal
 "The password is written down on a piece of paper. The terminal has access to heating and ventilation systems, including service access to reactor. There is also a lone executable with the name 'Martaanvuo'."
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
 'read-the-book
 '()
 #:decisions
 (list
  (make-decision
   #:title "Put the book down."
   #:description "Fuck it. She puts the book down."
   #:next-fragment 'exit
   )

  (make-decision
   #:title "Open the book."
   #:description "She flips open the cover."
   #:next-fragment 'recurse
   )))
