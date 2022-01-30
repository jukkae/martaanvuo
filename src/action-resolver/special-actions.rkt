#lang at-exp racket

(provide resolve-special-action!)

(require racket/lazy-require)

(require "../actions/action.rkt"
         "../core/io.rkt"
         "../core/utils.rkt"

         "../pc/pc.rkt")

(lazy-require
 ["../state/mutators.rkt"
  (flag-set?
   set-flag
   )])

(lazy-require
 ["../state/logging.rkt"
  (next-chapter!
   )])

(define (resolve-special-action! action)
  (case (action-symbol action)
    ['end-run
     (cond ((flag-set? 'ending-run-allowed)
            #;(p "At least it's something.")
            'end-run)
           (else
            (set-flag 'tried-to-go-back)
            (p @~a{
              Choking to death doesn't sound like a good time to Otava. Fuck it. She'll go find a gas mask.
            })
            (wait-for-confirm)
            (next-chapter!) ; end chapter, but not run!
            (p "Otava is getting close to what she's looking for, but she has trouble remembering how she got here. Did she follow the path of the Mediator? What was it that she was after?")
            (wait-for-confirm)
            (p "The Maw, the Monograph, the Cache, and the Gold. A single mind, laser-focused on four targets, one of which is the same as the other, ultimately, just two stages to both. Like, if you think about it, one's a way to freedom, one's a way to freedom, one's a way to a way to freedom, and one's a way to a way to freedom. One's a one way away from... Fucking hippies were right afterall, got to be free, man, 'cause otherwise what's the point? Die a fucking slave? Ha ha.")
            (p "This should be simple, Otava thinks.")
            (award-xp! 25 "for good thinking")
            'failure
            ))]

    ['back-off 'ok]
    ['win-game 'win-game]
    ['skip(cond ((member 'silent (action-details action))
              'ok)
             (else
              'ok))]
    [else (dev-note "unknown special action:")
          (displayln action)])

  )
