#lang racket

(require "actions.rkt")
(require "items.rkt")
(require "utils.rkt")

(define location<%>
  (interface ()
    get-description
    advance-to-next-description!
    get-interactions
    get-visible-neighbors))

(define location%
  (class* object% (location<%>)
    (init-field index)
    (field [descriptions (list "TODO: First description" "TODO: Second description" "TODO: Nth description")])
    (field [neighbors '()])

    
    (define times-described 0)
    (define searched? #f)
    
    (super-new)
    
    (define/public (get-description) (list-ref descriptions times-described))

    (define/public (advance-to-next-description!) (set! times-described (add1 times-described)))

    (define/public (get-interactions) (if searched?
                                          null
                                          (list (make-action 'search "Search the surroundings." 3 null '(wilderness)))))

    (define/public (get-visible-neighbors)
      '())

    (define/public (search)
      
      (define roll (d 2 6))
      (newline)
      (displayln (string-append "The area looks promising, so you take a look around." "[2d6: " (number->string roll) "]" ))
      
      (set! searched? #t)
      (define target-number 6)
      (define critical 10)
      (define loot (cond ((> roll critical) (new amulet%))
                         ((> roll target-number) (new knife%))
                         (else 'nothing)))
      loot)))

(provide (all-defined-out))