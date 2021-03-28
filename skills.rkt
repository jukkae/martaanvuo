#lang racket

(require "action.rkt")

(define (choice-from-symbol world pc symbol)
  (case symbol
    ['sleep 
     (make-choice 'sleep
                  "Sleep."
                  (λ () (make-action #:symbol 'sleep
                                     #:actor 'pc
                                     #:duration 100
                                     #:target null
                                     #:tags '(wilderness downtime))))]
    ['craft
     (make-choice 'craft
                  "Craft."
                  (λ () (make-action #:symbol 'craft
                                     #:actor 'pc
                                     #:duration 50
                                     #:target null
                                     #:tags '(wilderness downtime))))]
    ['forage
     (make-choice 'forage
                  "Forage."
                  (λ () (make-action #:symbol 'forage
                                     #:actor 'pc
                                     #:duration 100
                                     #:target null
                                     #:tags '(wilderness downtime))))]
    ['search
     (make-choice 'search
                  "Search."
                  (λ () (make-action #:symbol 'search
                                     #:actor 'pc
                                     #:duration 100
                                     #:target null
                                     #:tags '(wilderness downtime))))]
    ['inventory
     (make-choice 'inventory
                  "Show inventory. [free action]"
                  (λ () (make-action #:symbol 'inventory
                                     #:actor 'pc
                                     #:duration 0
                                     #:target null
                                     #:tags '(always free))))]
    ))

(define (get-downtime-choices world pc)
  (if (get-field in-combat world)
      '()
      (list (choice-from-symbol world pc 'forage)
            (choice-from-symbol world pc 'search)
            (if (not (eq? (get-field time-of-day world) 'night))
                (choice-from-symbol world pc 'craft)
                (choice-from-symbol world pc 'sleep))
            )))

(define (get-free-choices world pc)
  (list (choice-from-symbol world pc 'inventory)))

(define (get-choices world pc)
  (append (get-downtime-choices world pc)
          (get-free-choices world pc)))

(provide (all-defined-out))