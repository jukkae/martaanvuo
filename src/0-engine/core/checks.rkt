#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require
  "../actors/actor.rkt"
  "../core/io.rkt"
  "../2-core/core.rkt")

(lazy-require
 ["../martaanvuo.rkt"
  (
   pc)])

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
                          "success"
                          "failure"))
  (define wangernumb (list-ref wangernumbs wanger-index))
  (info-card
   (tbody
    (tr "luck" wangernumb)
    (tr "" result-text))
   "Luck check")
  (wait-for-confirm)
  result)

(provide passive-check)
(define (passive-check type comparator target-number . silent)
  (define text "")
  (case type
    ['charisma-mod (set! text (format "charisma mod > ~a" target-number))]
    ['fail-charisma-mod (set! text (format "fail charisma mod > ~a" target-number))]
    [else (error (format "passive check: unknown type: ~a" type))])

  (define attribute-value (get-trait (pc) "charisma"))
  (define modifier (get-attribute-modifier-for attribute-value))
  (define successful? (> modifier target-number))

  ; dirty but eh: for failures, flip successful here
  (case type
    ['fail-charisma-mod (set! successful? (not successful?))])

  (define result (if successful?
                     "check passed"
                     "check failed"))
  (define sheet
    (tbody
     (tr text
         (format "~a (~a)" attribute-value (get-modifier-string modifier))
         result)))

  (when (null? silent)
    (info-card
     sheet
     "Passive check"))

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
    (tbody
     (tr "1d20" "<" "attr")
     (tr
      (format "~a" roll)
      "<"
      (format "~a~a" attribute success-string))))

  (info-card
   results
   (format "Attribute check: ~a" title))

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
    (tbody
     (tr "2d6 + skill" ">=" "TN")
     (tr
      (format "~a + ~a + ~a = ~a" first-d second-d bonus roll-total)
      ">="
      (format "~a~a" target-number success-string))))

  (info-card
   results
   (format "Skill check: ~a" title))

  (wait-for-confirm)

  successful?)


;;; CRIT ROLL IDEA:
; Base chances are like 1/6 for "certain" attack failing, 1/6 for crit,
; but PC's attribute/skill bonuses can shift that balance
; - or would failures be based on saving throw? maybe.
(define (roll-crit? sides)
  (define crit-roll (d 1 sides))
  (define critical? (= crit-roll 6))
  (define crit-string (if critical?
                          ", crit"
                          ""))
  (notice (format "crit roll: 1d~a = ~a~a" sides crit-roll crit-string)))

