#lang at-exp racket

(provide (all-defined-out))

(require
  "../actors/actor.rkt"

  "../../1-index/state.rkt"

  "../../2-core/io.rkt"
  "../../2-core/core.rkt"

  "../../3-types/actor.rkt"
  )

(define (luck-check [target-number 7] [bonus 0])
  (define d1 (d 1 6))
  (define d2 (d 1 6))

  (define total (+ d1 d2 bonus))
  (define result (if (>= total target-number)
                     #t
                     #f))
  (define result-text (if result
                          "success"
                          "failure"))

  (notice (format "Luck check (~a): 2d6~a = ~a – ~a"
                  target-number
                  bonus
                  total
                  result-text))
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
  (define check-successful? (> modifier target-number))

  ; dirty but eh: for failures, flip successful here
  (case type
    ['fail-charisma-mod (set! check-successful? (not check-successful?))])

  (define result (if check-successful?
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
  check-successful?)

; returns boolean
(define (attribute-check title attribute)
  (define roll (d 1 20))
  (define check-successful? (< roll attribute))
  (define success-string
    (if check-successful?
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

  check-successful?)

; returns boolean
(define (skill-check title bonus target-number)
  (define first-d (d 1 6))
  (define second-d (d 1 6))
  (define roll-total (+ first-d second-d bonus))
  (define check-successful? (>= roll-total target-number))
  (define success-string
    (if check-successful?
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

  check-successful?)


(define (roll-crit? sides)
  (define crit-roll (d 1 sides))
  (define critical? (= crit-roll 6))
  (define crit-string (if critical?
                          ", crit"
                          ""))
  (notice (format "crit roll: 1d~a = ~a~a" sides crit-roll crit-string)))

(define (successful? check-result)
  (or (eq? check-result 'critical-success)
      (eq? check-result 'success)
      (eq? check-result 'narrow-success)))

; returns (U 'critical-success 'success 'narrow-success 'failure 'critical-failure)
(define (check formula #:title title #:target-number tn #:bonus [bonus '()])
  (notice (format "~a [~a >= ~a]" title formula tn))
  (wait-for-confirm)
  (cond [(eq? formula "2d6")
         (define first-d (d 1 6))
         (define second-d (d 1 6))
         (define bonus-string "")
         (when (null? bonus) (set! bonus 0))
         (define roll-total (+ first-d second-d bonus))
         (define check-outcome
           (cond
            [(and (= first-d 6) (= second-d 6)) 'critical-success]
            [(and (= first-d 1) (= second-d 1)) 'critical-failure]
            [(= roll-total tn) 'narrow-success]
            [(> roll-total tn) 'success]
            [else 'failure]
            ))
         (define success-string (format "~a" check-outcome))

         (for ([i tn])
          (display ".")
          (flush-output))
         (newline)
         (for ([i roll-total])
           (define sleep-time
             (cond [(>= i tn)
                    0.2]
                   [(= i (- tn 2))
                    (take-random (list 0.2 0.4 0.8 1.2))
                    ]
                   [(= i (- tn 1))
                    (take-random (list 0.4 0.8 1.2 1.6))]
                   [else
                    (take-random (list 0.2 0.4 0.8))]))
          (sleep sleep-time)
          (display ".")
          (flush-output)
          )
         (define sleep-time
           (cond
            [(> roll-total tn) 0.2]
            [else (take-random (list 0.4 0.8))]))
         (sleep sleep-time)
         (newline)
         (newline)

         (notice
          (format "[~a + ~a]~a = [~a >= ~a] [~a]" first-d second-d bonus-string roll-total tn success-string))

         (wait-for-confirm)

         check-outcome
         ]
        [else
         (dev-note (format "unknown formula ~a" formula))
         (error "unknown formula")]))
