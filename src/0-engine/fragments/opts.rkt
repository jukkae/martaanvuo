#lang at-exp racket

; shit implementation
(define (opts . params)
  (define ks '())
  (define vs '())
  (for ([i (length params)])
    (if (even? i)
        (set! ks (append-element ks (list-ref params i)))
        (set! vs (append-element vs (list-ref params i)))
        ))

  ; see also get-next-pc-action and build-keys-to-choices-map
  (define opts-with-keys (make-hash))
  (for ([i (in-range (length ks))])
    (hash-set! opts-with-keys (add1 i) (list-ref ks i))
    )

  (for ([(k v) (in-hash opts-with-keys)])
    (displayln (format "[~a]: ~a" k v)))

  (define input (wait-for-input))
  (define index (- (ann (string->number input) Natural) 1))
  (p (list-ref vs index)))