#lang typed/racket

(provide all-fulfill-predicate?)

(: all-fulfill-predicate? (∀ (A) (-> (Listof A) (-> A Boolean) Boolean)))
(define (all-fulfill-predicate? lst predicate)
  (define result #t)
  (for ([element lst])
    (when (not (predicate element))
      (set! result #f)))
  result)
