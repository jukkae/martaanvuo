#lang racket

(provide (all-defined-out))

(require "../decision.rkt"
         "../fragment.rkt"
         "../io.rkt"
         "../situation.rkt"
         "../utils.rkt")


(define (current-fragment-on-begin-round!)
  ((story-fragment-on-begin-round! (get-fragment (situation-current-fragment-id *situation*)))))

(define (current-fragment-get-decisions)
  (filter (lambda (potential-decision)
            ((decision-requirement potential-decision)))
          (story-fragment-decisions (get-fragment (situation-current-fragment-id *situation*)))))

(define (current-fragment-handle-decision! decision)

  
  (when (and (not (null? (decision-description decision)))
             (not (equal? (decision-description decision) "")))
    (p (decision-description decision)))
  

  (when (not (null? (decision-on-resolve! decision)))
    ((decision-on-resolve! decision)))
  
  (define next-fragment (decision-next-fragment decision))

  ; brilliant idea x dirty hack
  (when (procedure? next-fragment)
    (set! next-fragment (next-fragment)))
  (cond ((number? next-fragment)
         (go-to-story-fragment next-fragment)
         )
        
        ((symbol? next-fragment)
         (cond
           ; it can either be a special symbol...
           ((eq? 'exit next-fragment)
            (unset-current-fragment-id!))

           ((eq? 'recurse next-fragment)
            (unset-current-fragment-id!)
            'recurse) ; !! important
           
           ; ... or it can be just a label
           (else (go-to-story-fragment next-fragment))
           ))

        ((null? next-fragment) ; treat '() as 'exit
         (unset-current-fragment-id!)
         )
        
        (else (error (string-append "(current-fragment-handle-decision!): unexpected next-fragment type.")))))

(define (current-fragment-on-end-round!)
  '()
  )

(define (go-to-story-fragment id)
  (dev-note "HELLO")
  (dev-note (symbol->string id))
  (set-current-fragment-id! id)
  (dev-note (cond ((number? [situation-current-fragment-id *situation*])
                   (number->string [situation-current-fragment-id *situation*]))
                  ((null? [situation-current-fragment-id *situation*])
                   ("'()")))))

(define (handle-fragment-decision decisions-with-keys input)
  (define decision (hash-ref decisions-with-keys (string->number input)))
  (current-fragment-handle-decision! decision))