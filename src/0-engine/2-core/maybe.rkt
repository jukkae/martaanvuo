#lang typed/racket

(provide Maybe all-fulfill-predicate?)

(define-type (Maybe a) (U '() a))

; TODO: this belongs to its own file
(: all-fulfill-predicate? (∀ (A) (-> (Listof A) (-> A Boolean) Boolean)))
(define (all-fulfill-predicate? lst predicate)
  (define result #t)
  (for ([element lst])
    (when (not (predicate element))
      (set! result #f)))
  result)
