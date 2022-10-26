#lang at-exp racket

(require "../pc/pc.rkt"

         "../../3-types/action.rkt")

(define (choice-from-symbol world pc symbol)
  (case symbol
    ['sleep
     (make-choice 'sleep
                  "Sleep."
                  (λ ()
                    (make-action #:symbol 'sleep
                                 #:actor 'pc
                                 #:duration 200
                                 #:target null
                                 #:tags '(wilderness downtime))))]
    ['craft
     (make-choice 'craft
                  "Craft."
                  (λ ()
                    (make-action #:symbol 'craft
                                 #:actor 'pc
                                 #:duration 50
                                 #:target null
                                 #:tags '(wilderness downtime))))]
    ['eat
     (make-choice 'eat
                  "Eat."
                  (λ ()
                    (make-action #:symbol 'eat
                                 #:actor 'pc
                                 #:duration 10
                                 #:target null
                                 #:tags '(wilderness downtime))))]
    ['forage
     (make-choice 'forage
                  "Forage."
                  (λ ()
                    (make-action #:symbol 'forage
                                 #:actor 'pc
                                 #:duration 100
                                 #:target null
                                 #:tags '(wilderness downtime))))]
    ['search
     (make-choice 'search
                  "Search."
                  (λ ()
                    (make-action #:symbol 'search
                                 #:actor 'pc
                                 #:duration 100
                                 #:target null
                                 #:tags '(wilderness downtime))))]
    ['inventory
     (make-choice 'inventory
                  "Show inventory. [free action]"
                  (λ ()
                    (make-action #:symbol 'inventory
                                 #:actor 'pc
                                 #:duration 0
                                 #:target null
                                 #:tags '(always free))))]))

(define (get-free-choices world pc)
  (list (choice-from-symbol world pc 'inventory)))


(provide (all-defined-out))
