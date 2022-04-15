#lang at-exp racket

(provide (all-defined-out))

(require

  "../../2-core/io.rkt"
  "../../2-core/core.rkt"

  "../../4-systems/fragments/decision.rkt"
  "../../4-systems/fragments/fragment.rkt"

  "../../4-systems/fragments.rkt"
  "../../4-systems/timelines.rkt"
  "../../4-systems/simulation.rkt"

  "../../7-state/state.rkt"
  )


(define (current-fragment-on-begin-round!)
  ((story-fragment-on-begin-round! (get-fragment (current-fragment-id))))
  )

(define (current-fragment-get-decisions)
  (filter (lambda (potential-decision)
            ((decision-requirement potential-decision)))
          (story-fragment-decisions (get-fragment (current-fragment-id)))))

(define (current-fragment-handle-decision! decision)


  (when (and (not (null? (decision-description decision)))
             (not (equal? (decision-description decision) "")))
    (p (decision-description decision)))

  (define tl
    (advance-time-by-iotas!
     (decision-time-taken decision)))
  (process-timeline! tl)

  (when (not (null? (decision-on-resolve! decision)))
    ((decision-on-resolve! decision)))

  (define next-fragment (decision-next-fragment decision))

  ; brilliant idea x dirty hack
  (when (procedure? next-fragment)
    (set! next-fragment (next-fragment)))
  (cond ((number? next-fragment)
         (go-to-fragment next-fragment))

        ((symbol? next-fragment)
         (cond
           ; it can either be a special symbol...
           ((eq? 'exit next-fragment)
            (unset-current-fragment-id!))

           ((eq? 'recurse next-fragment)
            (unset-current-fragment-id!)
            'recurse) ; !! important

           ((eq? 'pc-dead next-fragment)
            (dev-note "PC DEAD NEXT FRAG")
            (unset-current-fragment-id!)
            'pc-dead)

           ; ... or it can be just a label
           (else (go-to-fragment next-fragment))))

        ((null? next-fragment) ; treat '() as 'exit
         (unset-current-fragment-id!))

        (else (error
               (format "(current-fragment-handle-decision!): unexpected next-fragment type: ~a"
                       next-fragment)))))

(define (current-fragment-on-end-round!)
  (current-completed-fragments
   (append-element (current-completed-fragments)
                   (current-fragment-id)))

  (define tl
    (advance-time-by-iotas!
     (story-fragment-time-taken-by-fragment ; TODO: This is broken, should happen on exiting from fragment, or on enter, or something
      (get-fragment (current-fragment-id)))))
  (process-timeline! tl)
  '()
  )

(define (handle-fragment-decision decisions-with-keys input)
  (define decision (hash-ref decisions-with-keys (string->number input)))
  (current-fragment-handle-decision! decision))