#lang at-exp racket

(provide (all-defined-out))

(require "../../2-core/io.rkt"
         "../../2-core/core.rkt"

         "../../3-types/decision.rkt"
         "../../3-types/fragment.rkt"

         "../../4-systems/fragments.rkt"
         "../../4-systems/timelines.rkt"
         "../../4-systems/simulation.rkt"

         "../../7-state/state.rkt")

(define (current-fragment-get-decisions)
  (filter (lambda (potential-decision) ((decision-requirement potential-decision)))
          (story-fragment-decisions (get-fragment (current-fragment-id)))))

(define (current-fragment-handle-decision! decision)

  (when (and (not (null? (decision-description decision)))
             (not (equal? (decision-description decision) "")))
    (p (decision-description decision)))

  (define tl (advance-time-by-iotas! (decision-time-taken decision)))
  (process-timeline! tl)

  (define next-fragment (decision-next-fragment decision))

  ; brilliant idea x dirty hack
  (when (procedure? next-fragment)
    (set! next-fragment (next-fragment)))
  (cond
    [(number? next-fragment) (go-to-fragment next-fragment)]

    [(symbol? next-fragment)
     (cond
       ; it can either be a special symbol...
       [(equal? 'exit next-fragment) (clear-current-fragment!)]

       [(equal? 'recurse next-fragment)
        (clear-current-fragment!)
        'recurse] ; !! important

       [(equal? 'pc-dead next-fragment)
        (dev-note "PC DEAD NEXT FRAG")
        (clear-current-fragment!)
        'pc-dead]

       ; ... or it can be just a label
       [else (go-to-fragment next-fragment)])]

    [(null? next-fragment) ; treat '() as 'exit
     (clear-current-fragment!)]

    [else
     (error (format "(current-fragment-handle-decision!): unexpected next-fragment type: ~a"
                    next-fragment))]))

(define (current-fragment-on-end-round!)
  (current-completed-fragments (append-element (current-completed-fragments) (current-fragment-id)))

  (define tl
    (advance-time-by-iotas!
     ; TODO: This is broken, should happen on exiting from fragment, or on enter, or something
     (story-fragment-time-taken-by-fragment (get-fragment (current-fragment-id)))))
  (process-timeline! tl)
  '())

(define (handle-fragment-decision decisions-with-keys input)
  (define decision (hash-ref decisions-with-keys (string->number input)))
  (current-fragment-handle-decision! decision))
