#lang at-exp racket

(provide (all-defined-out))

(require
  "../core/io.rkt"
  "../core/utils.rkt")

(define (blurb name)
  (case name
  ['begin-first-run
    (p @~a{
      Otava is following an old, overgrown path through the foggy woods. Late-morning gloom, forest like monochrome cardboard. Air thick with the damp smell of old, decaying forest.

      This should be worth it: There's a [cache] of valuables deep in the forest, but somewhere near it there's this basement lab too, a fucking abandoned junkie cellar kitchen, and if what she knows and what she's figured out is correct, she'll find the [Anthead Monograph] there.

      The first should net enough gold for Otava to pay back Mediator her debt anyway. That alone would be good enough a reason.

      The second one, though, Anthead Monograph, hoo. Her heart beats faster when she just thinks about it, the final key to her Transformation, and then none of this will matter anyway. The last step is to find the book that will fill in the blanks and make it all make sense. Oh hoh hoh, how she's understood all the pieces of the puzzle so far, how the toy box of reality turns, the tiny little cogs in the machine, how they all fit together! Spin the handle, insert flesh into the divine sausage machine, and out comes something magnificent:

      Otava the Seeker, become Otava the Deathless!
      })
    ]))
