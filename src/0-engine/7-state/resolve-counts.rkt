#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(lazy-require ["state.rkt"
               (current-times-begin-traverse-narrated current-times-finish-traverse-narrated
                                                      current-times-cancel-traverse-narrated)])

(define (times-begin-traverse-narrated++ key)
  (when (not (hash-has-key? (current-times-begin-traverse-narrated) key))
    (hash-set! (current-times-begin-traverse-narrated) key 0))
  (hash-set! (current-times-begin-traverse-narrated)
             key
             (add1 (hash-ref (current-times-begin-traverse-narrated) key))))

(define (times-begin-traverse-narrated key)
  (if (not (hash-has-key? (current-times-begin-traverse-narrated) key))
      #f
      (hash-ref (current-times-begin-traverse-narrated) key)))

(define (times-finish-traverse-narrated++ key)
  (when (not (hash-has-key? (current-times-finish-traverse-narrated) key))
    (hash-set! (current-times-finish-traverse-narrated) key 0))
  (hash-set! (current-times-finish-traverse-narrated)
             key
             (add1 (hash-ref (current-times-finish-traverse-narrated) key))))

(define (times-finish-traverse-narrated key)
  (when (not (hash-has-key? (current-times-finish-traverse-narrated) key))
    #f)
  (hash-ref (current-times-finish-traverse-narrated) key))

(define (times-cancel-traverse-narrated++ key)
  (when (not (hash-has-key? (current-times-cancel-traverse-narrated) key))
    (hash-set! (current-times-cancel-traverse-narrated) key 0))
  (hash-set! (current-times-cancel-traverse-narrated)
             key
             (add1 (hash-ref (current-times-cancel-traverse-narrated) key))))

(define (times-cancel-traverse-narrated key)
  (when (not (hash-has-key? (current-times-cancel-traverse-narrated) key))
    #f)
  (hash-ref (current-times-cancel-traverse-narrated) key))
