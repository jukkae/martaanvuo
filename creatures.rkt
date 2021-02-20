#lang racket

(define creature<%> (interface () get-name get-description get-next-action get-status hit))
(define bloodleech%
  (class* object% (creature<%>)
    (define hp 2)
    (super-new)

    (define/public (get-name) "Bloodleech")
    (define/public (get-description) "It is about the size of your forearm. Its moist skin glistens.")
    (define/public (get-next-action) (error "not implemented yet!"))
    (define/public (hit dmg) (displayln "OUCH"))
    (define/public (get-status) (error "not implemented yet!"))))

(define blindscraper%
  (class* object% (creature<%>)
    (define hp 1)
    (super-new)

    (define/public (get-name) "Blindscraper")
    (define/public (get-description) "You do not know why it is called a Blindscraper. You do not want to know.")
    (define/public (get-next-action) (error "not implemented yet!"))
    (define/public (hit dmg) (displayln "OUCH"))
    (define/public (get-status) (error "not implemented yet!"))))

(provide (all-defined-out))