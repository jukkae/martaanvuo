#lang at-exp racket

(provide (all-defined-out))

(require "../../../0-engine/0-api/api.rkt")

(require "../../../0-engine/3-types/choice.rkt")

; (Optional (Listof choice))
(define (get-perimeter-choices)
  (define actor (pc))

  (cond [(and (= (current-run) 1)
              (not (flag-set? 'tried-to-go-back))
              (not (> (pc-gold-amount) 0)))
          (choice
           'end-run
           "Go back to the bike."
           (make-action
           #:symbol 'try-and-fail-to-end-run
           #:actor actor
           #:duration 70
           #:tags '(downtime)
           #:resolution-rules
           `(
             (set-flag 'tried-to-go-back)
             (p @~a{
 Fuck it. Not worth it, she's not ready yet. Here's the, uh, it was a scouting trip to figure out the route. Which she did.
 })
             (wait-for-confirm)
             (next-chapter!) ; end chapter, but not run!
             (p "Otava can't shake the feeling of déjà vu, she's been here before. She's looking for the it the whatsit the hidebound book in the Maw and the Cache and the gold. The yeah, that's the book. The Maw's the miiiiiind. Ha ha ha. It's already seen.")
             (wait-for-confirm)
             (p "Find the Cache, take the Gold, pay the Debt, become Unfettered, find the Maw, find the Book, become Deathless.")
             (p "This should be simple, Otava thinks.")
             (award-xp! 25 "for good thinking")
             'failure))
           )
          ]
         [(> (pc-gold-amount) 0)
          (choice
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
           )
          ]
         [else '()])
  )
