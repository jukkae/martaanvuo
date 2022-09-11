#lang at-exp racket

(require
  "../pc-choices/choice.rkt"
  "../pc/pc.rkt"

  "../../3-types/action.rkt"
  )

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
    ['eat
     (make-choice 'eat
                  "Eat."
                  (λ () (make-action #:symbol 'eat
                                     #:actor 'pc
                                     #:duration 10
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
  (define choices '())
  (cond
    ((not (get-field in-combat world))
     (set! choices (cons (choice-from-symbol world pc 'forage) choices)) ; eurgh
     (set! choices (cons (choice-from-symbol world pc 'search) choices))
     (cond ((and (pc-hungry?)
                 (memq 'food (get-field inventory pc)))
            (set! choices (cons (choice-from-symbol world pc 'eat) choices))))
     (cond ((not (equal? (get-field time-of-day world) 'night))
            (set! choices (cons (choice-from-symbol world pc 'craft) choices))))
     (cond ((or (equal? (get-field time-of-day world) 'night)
                (equal? (get-field time-of-day world) 'evening))
            (set! choices (cons (choice-from-symbol world pc 'sleep) choices))))
     ))
  choices)

(define (get-free-choices world pc)
  (list (choice-from-symbol world pc 'inventory)))

(define (get-choices world pc)
  (append (get-downtime-choices world pc)
          (get-free-choices world pc)))

(provide (all-defined-out))
