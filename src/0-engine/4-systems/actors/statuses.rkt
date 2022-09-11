#lang at-exp racket

(provide (all-defined-out))

(require
  "../../2-core/io.rkt"
  "../../2-core/core.rkt"
  "../../3-types/status.rkt"
  "../../3-types/actor.rkt"
  )

(define (actor-add-status! actor status)
  (when (not (null? actor))
    (cond [(not (actor-has-status-of-type? actor (status-type status)))
           (notice (format "~a: Status ~a [~a turns] added" (actor-name actor) (status-type status) (status-lifetime status)))
           (set-actor-statuses! actor (append-element (actor-statuses actor) status))
           ]
          [else
           (modify-actor-status-lifetime actor (status-type status) (status-lifetime status))
           (notice
            (format "~a: Lifetime of status ~a extended by ~a ~a, new lifetime: ~a."
              (actor-name actor)
              (status-type status)
              (status-lifetime status)
              (if (= (status-lifetime status) 1) "turn" "turns")
              (actor-lifetime-of-status-of-type? actor (status-type status))
            ))
          ]
    )
    )
  )

(define (actor-has-status-of-type? actor type)
  (if (memf (λ (status)
              (equal? (status-type status) type))
            (actor-statuses actor))
      #t
      #f))

(define (actor-lifetime-of-status-of-type? actor type)
  (define s (findf (λ (status)
                     (equal? (status-type status) type))
                   (actor-statuses actor)))
  (if s
      (status-lifetime s)
      #f))

(define (decrement-actor-status-lifetimes! actor)
  (for ([status (actor-statuses actor)])
    (set-status-lifetime! status (- (status-lifetime status) 1)))
  (define new-statuses '())
  (for ([status (actor-statuses actor)])
    (if (not (negative? (status-lifetime status)))
        (set! new-statuses (append-element new-statuses status))
        (notice
         (format "~a: Status [~a] removed"
                 (actor-name actor)
                 (status-type status)))))
  (set-actor-statuses! actor new-statuses))

; think:
; what if statuses are, by definition, something that's a fairly explicit list?
; and much of combat control is based on manipulating statuses?
(define (modify-actor-status-lifetime actor type modify-amount)
  (for ([status (actor-statuses actor)])
    (when (equal? (status-type status) type)
      ; (notice (format "~a: Status [~a] modified" (actor-name actor) (status-type status)))
      (set-status-lifetime! status (+ (status-lifetime status) modify-amount))))
  (define new-statuses '())
  (for ([status (actor-statuses actor)])
    (if (positive? (status-lifetime status))
        (set! new-statuses (append-element new-statuses status))
        (notice (format "~a: Status [~a] removed" (actor-name actor) (status-type status)))))
  (set-actor-statuses! actor new-statuses))

; yeah the way statuses currently work are a piece of shit
; but the idea of strength decreasing by one always at end of turn, as well as conditionally,
; it's a good idea
(define (actor-set-status! actor type value)
  (when (not (null? actor))
    (notice (format "~a: [~a]: ~a" (actor-name actor) type value)))

  (if (actor-has-status-of-type? actor type)
      (for ([status (actor-statuses actor)])
        (when (equal? (status-type status) type)
          (set-status-lifetime! status value)))
      (actor-add-status! actor (status type value))))

(define (actor-remove-status! actor status)
  (when (not (null? actor))
    (notice (format "~a: Status [~a] removed" (actor-name actor) (status-type status))))
  (set-actor-statuses! actor (remove status (actor-statuses actor))))

(define (actor-remove-status-of-type! actor type)
  (when (not (null? actor))
    (when (actor-has-status-of-type? actor type)
      (set-actor-statuses! actor (filter
                                (λ (other) (not (equal? type
                                                     (status-type other))))
                                (actor-statuses actor)))
      (notice (format "~a: Status [~a] removed" (actor-name actor) type))
      )))
