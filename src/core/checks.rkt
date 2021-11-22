#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require
  "../actors/actor.rkt"
  "../core/io.rkt"
  "../core/utils.rkt")

(lazy-require
 ["../martaanvuo.rkt"
  (
   pc
   )])

(define (luck-check)
  (define wangernumbs
    (list
     "i"
     "8-4"
     "21.3"
     ".01"
     "41"
     "9Ɛ"
     "12"
     "70"
     "26"
     "2"
     "-3"
     "±6"))
  (define wanger-index (random (length wangernumbs)))
  (define result (if (< wanger-index (/ (length wangernumbs) 2))
                     #t
                     #f))
  (define result-text (if result
                          " success "
                          " failure "))
  (define wangernumb (list-ref wangernumbs wanger-index))
  (info-card
   (list
    (list " luck " (string-append " " wangernumb " "))
    (list " " result-text))
   "Luck check")
  (wait-for-confirm)
  result)

(provide passive-check)
(define (passive-check type comparator target-number . silent)
  (define text "")
  (case type
    ['charisma-mod (set! text (string-append "charisma mod > " (number->string target-number)))]
    ['fail-charisma-mod (set! text (string-append "fail charisma mod > " (number->string target-number)))]
    [else (error (string-append "passive check: unknown type: " (symbol->string type)))])

  (define attribute-value (get-trait (pc) "charisma"))
  (define modifier (get-attribute-modifier-for attribute-value))
  (define successful? (> modifier target-number))

  ; dirty but eh: for failures, flip successful here
  (case type
    ['fail-charisma-mod (set! successful? (not successful?))]
    )
  
  (define result (if successful?
                     "check passed"
                     "check failed"))
  (define sheet
    (list
     (list (string-append " " text " ")
           (string-append " " (number->string attribute-value) " (" (get-modifier-string modifier) ") ")
           (string-append " " result " "))
     
     ))
  (when (null? silent)
    (info-card
     sheet
     "Passive check"
     ))
  (wait-for-confirm)
  successful?)

; returns boolean
; (eventually: 'critical-success and 'critical-failure?)
(define (attribute-check title attribute)
  (define roll (d 1 20))
  (define successful? (< roll attribute))
  (define success-string
    (if successful?
        ", success"
        ", failure"))
  (define results
    (list
     (list " 1d20 " " < " " attr ")
     (list
      (string-append
       " "
       (number->string roll))
      " < "
      (string-append " " (number->string attribute) success-string " "))))
               
  (info-card
   results
   (string-append "Attribute check: " title))

  (wait-for-confirm)

  successful?)

; returns boolean
(define (skill-check title bonus target-number)
  (define first-d (d 1 6))
  (define second-d (d 1 6))
  (define roll-total (+ first-d second-d bonus))
  (define successful? (>= roll-total target-number))
  (define success-string
    (if successful?
        ", success"
        ", failure"))
  (define results
    (list
     (list " 2d6 + skill " " >= " " TN ")
     (list
      (string-append
       " "
       (number->string first-d)
       "+"
       (number->string second-d)
       "+"
       (number->string bonus)
       " = "
       (number->string roll-total))
      " >= "
      (string-append " " (number->string target-number) success-string " "))))
               
  (info-card
   results
   (string-append "Skill check: " title))

  (wait-for-confirm)

  successful?)