#lang racket

(provide print-choices-and-meta-commands-with-keys
         print-meta-commands-with-keys
         meta-command-valid?)

(require "../choice.rkt"
         "../decision.rkt")

(define (print-decisions-with-keys decisions-with-keys)
  (for ([(k v) (in-hash decisions-with-keys)])
    (displayln (string-append "[" (number->string k) "]: " (decision-title v))))
  #;(newline))

(define (print-choices-with-keys choices-with-keys)
  (define choices
    (for/list ([(k v) (in-hash choices-with-keys)])
      (cons k v)))
  
  (set! choices
        (sort choices
              (λ (c1 c2) (< (car c1) (car c2)))))
  
  (for ([choice choices])
    (displayln (string-append "[" (number->string (car choice)) "]: " (choice-name (cdr choice)))))
  (newline))

(define (print-meta-commands-with-keys meta-commands-with-keys)
  (for ([(k v) (in-hash meta-commands-with-keys)])
    (display (car v))
    (display " "))
  (newline)
  (newline))

(define (print-choices-and-meta-commands-with-keys
         choices-with-keys
         fragment-decisions-with-keys
         meta-commands-with-keys verbosity)
  (cond ((eq? verbosity 'abbreviated)
         (display "Unknown command. Known commands: ")
         (for ([(k v) (in-hash fragment-decisions-with-keys)]) (display k))
         (for ([(k v) (in-hash choices-with-keys)]) (display k))
         (for ([(k v) (in-hash meta-commands-with-keys)]) (display k))
         (newline)
         )
        (else
         (newline) ; This is the extra space, should pass a param to print call perhaps instead?
         #;(p "What do you do?")
         (print-decisions-with-keys fragment-decisions-with-keys)
         (print-choices-with-keys choices-with-keys)
         (print-meta-commands-with-keys meta-commands-with-keys))))

(define (meta-command-valid? meta-commands-with-keys input)
  (set! input (string-upcase input))
  (define meta-command (hash-ref meta-commands-with-keys input '()))
  (if (not (null? meta-command))
      meta-command
      #f))