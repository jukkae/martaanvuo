#lang racket

(define creature<%> (interface () get-name get-description get-next-action get-status hit))
(define bloodleech%
  (class* object% (creature<%>)
    (field [hp 2])
    (field [defense 5])
    (super-new)

    (define/public (get-name) "Bloodleech")
    (define/public (get-description) "It is about the size of your forearm. Its moist skin glistens.")
    (define/public (get-next-action) (error "not implemented yet!"))
    (define/public (hit dmg)
      (begin (set! hp (- hp dmg))
             (if (<= hp 0)
                 (begin (set! hp 0)
                        'dead)
                 'hit)))
    (define/public (get-status) (error "not implemented yet!"))))

(define blindscraper%
  (class* object% (creature<%>)
    (field [hp 1])
    (field [defense 9])
    (super-new)

    (define/public (get-name) "Blindscraper")
    (define/public (get-description) "You do not know why it is called a Blindscraper. You do not want to know.")
    (define/public (get-next-action) (error "not implemented yet!"))
    (define/public (hit dmg)
      (begin (set! hp (- hp dmg))
             (if (<= hp 0)
                 (begin (set! hp 0)
                        'dead)
                 'hit)))
    (define/public (get-status) (error "not implemented yet!"))))

(provide (all-defined-out))