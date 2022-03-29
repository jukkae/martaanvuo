#lang at-exp racket

(provide (all-defined-out))

(require "../../../0-engine/0-api/api.rkt")

(define (get-perimeter-choices)
  (choice
    'end-run
    "Go back home."
    (make-action
      #:symbol 'end-run
      #:actor actor
      #:duration 100
      #:tags '(downtime)
      #:resolution-rules
      `(
        (cond ((flag-set? 'ending-run-allowed)
               (p "At least it's something.")
              'end-run)
              (else
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
               'failure
              ))))

    ))
