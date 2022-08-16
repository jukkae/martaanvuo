#lang at-exp racket

(provide (all-defined-out))

(require
  "../../0-engine/2-core/io.rkt"
  "../../0-engine/2-core/core.rkt"
  "../../0-engine/4-systems/blurbs/blurbs.rkt"
  "../../0-engine/7-state/state.rkt"
  )

(define (narrate-restart)
  (p
   (take-random
    (list
     "Unpruned, abandoned, the story ends here."
     "Unpruned, abandoned, this story ends here."
     "Unpruned, abandoned, the story of Otava ends here."
     "Unpruned, abandoned, this story of Otava ends here."
     "Unpruned, abandoned, the story of this Otava ends here."
     "Unpruned, abandoned, this story of this Otava ends here."
     "In the depths of the Maw, the heart of the world stops."
     (get-blurb 'the-end))
    #:distribution 'quadratic)))

(define (display-playthrough-stats)
  (define body
    (tbody
      (tr "MARTAANVUO")
      (tr "")
      (tr "Otava did not become deathless.")
      ; (tr "Otava did not pay back her debt.")
      (tr "Otava did not escape Martaanvuo.")
      (tr "The world continues to exist.")
      (tr "")
      (tr (format "Otava lived ~a ~a."
                  (current-life)
                  (if (= (current-life) 1) "life" "lives")
                  ))
      ; (tr (format "Otava ran ~a ~a and lived ~a ~a."
      ;             (current-run)
      ;             (if (= (current-run) 1) "run" "runs")
      ;             (current-life)
      ;             (if (= (current-life) 1) "life" "lives")
      ;             ))
      (tr "")
      (tr (format "[Random seed: ~a]" (current-rng-seed)))
      ))
  (newline)
  (info-card body '()))
