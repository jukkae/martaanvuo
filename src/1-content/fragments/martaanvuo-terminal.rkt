#lang at-exp racket

(provide (all-defined-out))

(require "../../0-engine/0-api/api.rkt")

(fragment 'turn-on-martaanvuo-terminal
          "There is a lone executable with the name 'Martaanvuo'."
          #:decisions #;(make-decision
                         #:title "HVAC and service access to reactor."
                         #:description "Otava takes a glance at the familiar-looking systems."
                         #:next-fragment 'exit)
                      (list (make-decision #:title "Launch Martaanvuo."
                                           #:description "The terminal greets her:"
                                           #:next-fragment 'recurse)
                            (make-decision #:title "Close the terminal."
                                           #:description "Otava closes the terminal."
                                           #:next-fragment 'exit)))

(fragment 'read-the-book
          '()
          #:decisions (list (make-decision #:title "Put the book down."
                                           #:description "Fuck it. She puts the book down."
                                           #:next-fragment 'exit)
                            (make-decision #:title "Open the book."
                                           #:description "She flips open the cover."
                                           #:next-fragment 'recurse)))
