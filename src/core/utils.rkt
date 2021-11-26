#lang racket

(provide (all-defined-out))

(require racket/path) ; used in (here)

; dice shorthand
(define (d n sides)
  (for/sum ([i n])
    (add1 (random sides))))

; list utils
(define (take-n-random l n)
  (take (shuffle l) n))

(define (slice l offset n)
  (take (drop l offset) n))

(define (take-random l)
  (list-ref l (random (length l))))

(define (append-element lst elem)
  (append lst (list elem)))

(define (reduce lst func)
  (when (null? lst) (error "reduce: lst cannot be '()"))
  (if (null? (cdr lst))
      (car lst)
      (func (car lst) (reduce (cdr lst) func))))

(define (all-fulfill-predicate? lst predicate)
  (define result #t)
  (for ([element lst])
    (when (not (predicate element))
      (set! result #f)))
  result)

(define (prune lst)
  (filter
   (λ (x) (and (not (null? x))
               (not (void? x))))
   lst))

(define (condense lst)
  (filter
   (λ (x) (and (not (null? x))
               (not (void? x))))
   (flatten lst)))

; DSL macros
(define-syntax-rule
  (nop)
  (λ () '()))

#;(define (once fn)
  (let ((called? #f))
    (λ ()
      (if (not called?)
        (fn)
        (prln "this has been called already"))
      (set! called? #t))))

; __FILE__ and __LINE__ all in one
(define-syntax (here stx)
  (with-syntax ([file (syntax-source stx)]
                [line (syntax-line stx)])
    (syntax-case stx ()
      [_ #'(begin
             (displayln "HERE")
             (displayln line)
             (displayln (file-name-from-path file)))])))

(define-syntax (dev-note stx)
  (with-syntax ([file (syntax-source stx)]
                [line (syntax-line stx)])
    (syntax-case stx ()
      [(dev-note message)
       #'(begin
           (displayln
            (format "<~a:~a> ~a"
                    (path->string (find-relative-path (current-directory) file))
                    (number->string line)
                    message)))])))

; various container stuff
(define (collect-similar lst)
  (hash->list
   (foldl (lambda (key ht)
            (hash-update ht key add1 0))
          '#hash()
          lst)))
