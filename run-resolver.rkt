#lang racket

(provide (all-defined-out))

(require "io.rkt")
(require "round-resolver.rkt")
(require "situation.rkt")
(require "world.rkt")

; content should be provided "somewhere"
; content is game-specific, not engine stuff
; but figure out where this should be called from
; engine / run-resolver
(define (narrate-begin-run)
  (info-card
   (list
    (list " run " (string-append " " (number->string (situation-run *situation*)) " ")))
   (string-append "Begin run number " (number->string (situation-run *situation*))))
  (case (situation-run *situation*)
    [(1)
     (paragraph "After a couple of days of hiking, the old blacktop road Otava has been following starts to descend towards Martaanvuo swamps. The Collector told her of a pre-Rains laboratory there, abandoned and forgotten, just inside the Anomaly. There's a small reactor in the basement, and that should be enough to settle her debt.")]
    [(2)
     (paragraph "As the path descends, temperature climbs, and Otava soon finds herself drenched in sweat.")]))


; engine / run-resolver
(define (on-begin-run)
  (set-situation-run! *situation* (add1 (situation-run *situation*)))
  (set-situation-round! *situation* 0)
  (move-actor-to-location! (situation-pc *situation*) edgeflats)
  (narrate-begin-run)
  (go-to-story-fragment 1)
  )

; engine / run-resolver
(define (resolve-a-run)
  (on-begin-run)
  (let/ec end-run
    (let loop ()
      (define round-exit-status (resolve-round))
      (when (eq? round-exit-status 'pc-dead) (end-run 'pc-dead))
      (when (eq? round-exit-status 'win-game) (end-run 'win-game))
      (when (eq? round-exit-status 'end-run) (end-run 'end-run))
      (loop))
    ))