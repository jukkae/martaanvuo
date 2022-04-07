#lang at-exp racket

(provide (all-defined-out))

(require
  "../0-engine/0-api/api.rkt"
  "narration/run-resolver.rkt"
)

(define (on-begin-run #:suppress-new-chapter? [suppress-new-chapter? #f])
  (current-run (add1 (current-run)))
  (current-round 0)
  (move-pc-to-location! (get-place-by-id 'perimeter))
  (narrate-begin-run #:suppress-new-chapter? suppress-new-chapter?)

  (case (current-run)
    [(1)
     (blurb 'begin-first-run-pt-1)
     (create-task 'the-debt)
     (wait-for-confirm)
     (blurb 'begin-first-run-pt-2)
     (create-task 'anthead-monograph)
     (wait-for-confirm)
     ]
    )
  )

(define (on-end-run exit-status)
  (reset-pending-action!)
  (when (and (not (eq? exit-status 'restart))
             (not (eq? exit-status 'recurse)))
    (cond ((> (pc-gold-amount) 0)
           (define debt-task (task-exists? 'the-debt))
           (define gold-collected (pc-gold-amount))
           (dev-note (format "TODO: reduce debt by ~a" gold-collected))
           (remove-item! 'gold)

           (displayln "Task:")
           (displayln debt-task)

           (display-run-summary))

          (else
           (notice (format "End run number ~a [failed]" (number->string (current-run)))))))

  (case exit-status
    ['end-run
     (p "She's still alive.")]
    ['recurse
     (blurb 'martaanvuo-title)]
    ['restart
     '()]
    ['pc-dead '()]
    [else
     (dev-note (format "on-end-run: unhandled exit status: ~a" exit-status))])

  (when (not (eq? exit-status 'pc-dead))
    (wait-for-confirm)))