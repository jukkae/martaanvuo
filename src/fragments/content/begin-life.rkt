#lang at-exp racket

(provide (all-defined-out))

(require "../../core/api.rkt")


(fragment
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
'begin-life-exit
(thunk
  (p @~a{
    The forest she's walking through has a jagged metal shard underbrush. Some of it makes sense: A malevolent shiny black watcher-eye thing, a slither-snaking soul-sucking what's-the-fancy-word a cable and a movement in the corner of her eye catches her eye in the corner of her eye.

    The flickering static of a screen, all jumbled and incoherent, and then something changes and there's order where there previously was only chaos. Atoms form from the proverbial soup, and then there are forms and forces and structures, and then there's the woods and the silhouette trees, and there's a sign and the sign says ANOMALY PERIMETER.

    She is on a path, and the path splits.
  })
  (wait-for-confirm)
  (p @~a{
    A magpie calls from high up the rocky hill on the left. An army of ants is marching down, towards Martaanvuo swamp.
  })
  ))
