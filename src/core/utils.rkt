#lang at-exp racket

(provide (all-defined-out))

(require racket/path) ; used in (here)
(require racket/lazy-require)

(lazy-require
 ["io.rkt"
  (p
   wait-for-input)])


; dice shorthand
(define (d n sides)
  (for/sum ([i n])
    (add1 (random sides))))


; list utils
(define (take-n-random l n)
  (take (shuffle l) n))

(define (slice l offset n)
  (take (drop l offset) n))

; distributions:
; 'constant: every value equally likely
; 'quadratic: every value half as likely as the previous one
(define (take-random l #:distribution [distribution 'constant])
  (case distribution
   ['constant
    (list-ref l (random (length l)))]
   ['quadratic
    (define n (length l))
    (define max_roll (- (expt 2 n) 1))
    (define roll (d 1 max_roll))
    (define index (- (- n (exact-floor (log roll 2))) 1))
    (list-ref l index)
    ]
   [else (error (format "Unknown distribution ~a" distribution))])
  )

(define (append-element lst elem)
  (append lst (list elem)))

(define (reduce lst func)
  (when (null? lst) (error "reduce: lst cannot be '()"))
  (if (null? (cdr lst))
      (car lst)
      (func (car lst) (reduce (cdr lst) func))))

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

(define (insert-at lst pos x)
  (define-values (before after) (split-at lst pos))
  (append before (cons x after)))


; String utils
(define (capitalize-first-letter str)
  (cond
    [(non-empty-string? str)
     (define first-letter-str
       (substring str 0 1))
     (define rest-str
       (substring str 1 (string-length str)))
     (string-append (string-upcase first-letter-str)
                    rest-str)]
    [else
     ""]))



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
            (format "< ~a:~a > ~a"
                    (path->string (find-relative-path (current-directory) file))
                    (number->string line)
                    message)))])))


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
  (define index (- (string->number input) 1))
  (p (list-ref vs index)))


; various container stuff
(define (collect-similar lst)
  (hash->list
   (foldl (lambda (key ht)
            (hash-update ht key add1 0))
          '#hash()
          lst)))


; menus - rename this!
(define (handle-meta-command meta-commands-with-keys input)
  (set! input (string-upcase input))
  (define meta-command-with-key (hash-ref meta-commands-with-keys input '()))
  (define meta-command (cdr meta-command-with-key))
  (meta-command))
