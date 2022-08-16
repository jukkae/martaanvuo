#lang at-exp racket

(provide (all-defined-out))

(require "../../../0-engine/0-api/api.rkt")

; (Optional (Listof choice))
(define (get-perimeter-choices)
  (define actor (pc))

  (cond
   #;[(> (pc-gold-amount) 0)
    (list (choice
      'end-run
      "Go back to the bike."
      (make-action
      #:symbol 'end-run
      #:actor actor
      #:duration 40
      #:tags '(downtime)
      #:resolution-rules
      `(
        (p "Otava goes back to her bike.")
        'end-run))
      ))
    ]
   [else '()]))
