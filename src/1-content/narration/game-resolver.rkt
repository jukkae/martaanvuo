#lang at-exp racket

(provide (all-defined-out))

(require
  "../../0-engine/2-core/io.rkt"
  "../../0-engine/2-core/core.rkt"
  "../../0-engine/4-rules/blurbs/blurbs.rkt"
  "../../0-engine/7-state/state/state.rkt"
  )

(define (narrate-restart)
  (p
   (take-random
    (list
     "An unfinished timeline continues its chaotic growth, an eldritch tentacle tying itself in a thousand knots through dimensions of time and space."
     "Unpruned, abandoned, the story ends here."
     "Unpruned, abandoned, the story of Otava ends here."
     "Unpruned, abandoned, the story of this Otava ends here."
     "Unpruned, abandoned, this story of this Otava ends here."
     "In the depths of the Maw, the Heart of the World stops."
     (get-blurb 'the-end)
     "[data missing - 13 days later] Otava comes upon a mountain range. She finds a way over, and begins a new life on the other side, deep within the vast unnamed forests.")
    #:distribution 'quadratic)))

(define (display-playthrough-stats)
  (define body
    (tbody
      (tr "MARTAANVUO")
      (tr "")
      (tr "Otava did not become deathless.")
      (tr "Otava did not pay back her debt.")
      (tr "Otava did not escape Martaanvuo.")
      (tr "The world continues to exist.")
      (tr "")
      (tr (format "Otava ran ~a ~a and lived ~a ~a."
                  (current-run)
                  (if (= (current-run) 1) "run" "runs")
                  (current-life)
                  (if (= (current-life) 1) "life" "lives")
                  ))
      ))
  (newline)
  (info-card body '()))
