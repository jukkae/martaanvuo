#lang at-exp racket

(provide (all-defined-out))

(require "../../core/api.rkt")


#;(fragment
 'begin-life
 (thunk
  (p @~a{
     She goes over her checklist again: Plan? Part of a plan, yes. Knife? Yes. Provisions? For a couple of days, yes. Bolt cutters? Yes.
    }))
 #:decisions
 (list
  (make-decision
   #:title "Rope."
   #:description "Rope, a length of, at least half of which is mostly not entirely frayed."
   #:on-resolve! (thunk
                  (set-build! 'rope)
                  (wait-for-confirm)

                  (current-show-round-summary? #t))
   #:next-fragment 'begin-life-exit
   )

  (make-decision
   #:title "Flashlight."
   #:description "A flashlight, with almost half of a full charge."
   #:on-resolve! (thunk
                  (set-build! 'flashlight)
                  (wait-for-confirm)

                  (current-show-round-summary? #t))
   #:next-fragment 'begin-life-exit)
  )
 )

(fragment
'begin-life
(thunk
  (p @~a{
    The forest she's walking through has a jagged metal shard underbrush. Some of it makes sense: A malevolent shiny black watcher-eye thing, a slither-snaking soul-sucking what's-the-fancy-word a cable and a movement in the corner of her eye catches her attention.

    Oh it's just the flickering static of a screen, jumbled and incoherent chaos, and then there's just black. Then, atoms emerge, and then there are forms and forces and structures, and then there's the forest and the ants and the silhouette trees, and Otava is walking through a forest.

    She is on a path, and the path splits.
  })
  (wait-for-confirm)
  (p @~a{
    A magpie calls from high up the rocky hill on the left. There's a natural staircase leading up.

    A small squad of ants is marching down the narrow, barely noticeable right-hand trail that's sloping down towards Martaanvuo swamp.

    The air is not right here, it's like she draws it in but it isn't *enough*, like there's too much filth and rottenness and something wet and dirty and heavy in it. Otava's chest feels tight.
  })
  ))
