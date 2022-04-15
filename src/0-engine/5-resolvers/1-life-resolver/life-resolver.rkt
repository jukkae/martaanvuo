#lang at-exp racket

;;; (provide (all-defined-out))

;;; (require
;;;   "../2-run-resolver/run-resolver.rkt"
;;;   "../3-round-resolver/round-resolver.rkt"

;;;   "../../1-index/content.rkt"

;;;   "../../2-core/io.rkt"
;;;   "../../2-core/core.rkt"
;;;   "../../2-core/session.rkt"

;;;   "../../3-types/pc-actor.rkt"

;;;   "../../4-systems/pc/pc.rkt"

;;;   "../../7-state/state.rkt"

;;;   "../../../1-content/narration/gameplay-transitions.rkt"
;;;   )


;;; (define (resolve-life mode)
;;;   (case mode
;;;     ['begin (on-begin-life)]
;;;     ['restart (on-begin-life)])

;;;   (let/ec end-life
;;;     (let loop ([m mode])

;;;       ; first run of the life
;;;       (define first-run?
;;;         (case m
;;;           ['begin #t]
;;;           ['restart #t]
;;;           ['else #f]))

;;;       (define run-exit-status
;;;         (resolve-run m #:suppress-new-chapter? (not first-run?)))

;;;       (case run-exit-status
;;;         ['pc-dead (on-end-life)
;;;          (end-life 'pc-dead)]
;;;         ['win-game (end-life 'win-game)]
;;;         ['end-run (loop 'begin)]
;;;         ['recurse (loop 'recurse)]
;;;         ['restart (end-life 'restart)])
;;;       )
;;;     ))
