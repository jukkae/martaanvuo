#lang at-exp racket

(provide (all-defined-out))

(require "../../../0-engine/0-api/api.rkt")

(require "../../../0-engine/3-types/choice.rkt")

; (Optional (Listof choice))
(define (get-perimeter-choices)
  (define actor (pc))

  (dev-note (format "flags: ~a" (current-flags)))
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
             (p "Otava is getting close to what she's looking for, but she has trouble remembering how she got here. Did she follow the path of the Mediator? What was it that she was after?")
             (wait-for-confirm)
             (p "The Maw, the Monograph, the Cache, and the Gold. A single mind, laser-focused on four targets, one of which is the same as the other, ultimately, just two stages to both. Like, if you think about it, one's a way to freedom, one's a way to freedom, one's a way to a way to freedom, and one's a way to a way to freedom. One's a one way away from... Fucking hippies were right afterall, got to be free, man, 'cause otherwise what's the point? Die a fucking slave? Ha ha.")
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
