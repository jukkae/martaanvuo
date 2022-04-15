#lang at-exp racket

(provide (all-defined-out))

(require
  "../../../../version.rkt"

  "../../../2-core/io.rkt"
  "../../../2-core/core.rkt"
  "../../../2-core/session.rkt"

  "../../../3-types/task.rkt"

  "../../../4-systems/blurbs/blurbs.rkt"

  "../../../7-state/state.rkt"
  )

(define (display-session-stats)
  (newline)
  (define session-score (d (current-session-score-dice) 4))
  (define body
    (tbody
     (tr (format "Total score: ~a" session-score))
     (tr "")
     (tr (format "~a" (current-session-score-reasons)))))
  (info-card body "Session stats")
  (newline))


; Return value is tied to round resolution.
(define (quit)
  (displayln "Your attention is the thin barrier between the world and the void. Really quit?")
  (newline)
  (displayln "[Q] to quit, anything else to continue.")
  (define input (wait-for-input))
  (set! input (string-upcase input))
  (cond ((equal? input "Q")
         (display-session-stats)

         (define quit-message (get-blurb 'quit))
         (when (not (equal? quit-message ""))
           (prln quit-message)
           (newline))

         (define (print-version)
           (prln (format "[Martaanvuo v~a]" martaanvuo-version)))

         (print-version)

         (exit))
        (else
         (newline)
         #t))) ; mark input as handled

(define (restart)
  (displayln "Really restart?")
  (newline)
  (displayln "[R] to restart, anything else to continue.")
  (define input (wait-for-input))
  (set! input (string-upcase input))
  (cond ((equal? input "R")
         'restart)
        (else
         (newline)
         #t))) ; mark input as handled


(define (inventory)
  (inventory)
  #t)

(define (display-tasks)
  (info-card
   (for/list ([t (current-tasks)])
     (tr
      (~a (task-name t))
      (~a (task-info-blurb t))
      (~a (task-status-text t))))
   "Tasks")
  (wait-for-confirm))

(define (display-log)
  (hr)
  (prln "[BEGIN LOG]")
  (newline)
  (display-title)
  #;(prln (current-log))
  (for ([entry (current-log)])
    (print-paragraph (format-for-printing entry #:width 84 #:indent 4)))
  (prln "[END LOG]")
  (newline)
  (wait-for-confirm))