#lang at-exp racket

(provide (all-defined-out))

(require
  "../actors/actor.rkt"

  "../../1-index/state.rkt"

  "../../2-core/maybe.rkt" ; TODO: for all-fulfill-predicate
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
  (or (equal? check-result 'critical-success)
      (equal? check-result 'success)
      (equal? check-result 'narrow-success)))

(define (just-roll formula #:title title #:bonus [bonus '()] #:on-critical-success [on-crit-success '()] #:on-critical-failure [on-crit-failure '()])
  (cond [(string=? formula "2d6")
         (define n 2)
         (define sides 6)
         (define results (for/list ([i n])
           (d 1 sides)))
         (define bonus-string "")
         (cond [(null? bonus) (set! bonus 0)]
               [else
                (set! bonus-string
                      (if (< bonus 0)
                          (format "- ~a" (abs bonus))
                          (format "+ ~a" bonus)))])
         (define roll-total (+ (apply + results) bonus))

         (define total-printed 0)

         (define base-sleep-time 0.05)
         (display (format "~a ~a " title formula))
         (sleep (* 2 base-sleep-time))
         (for ([dice results])
           (for ([i dice])
             (if (= i 0)
                 (display ":")
                 (display "."))
             (set! total-printed (add1 total-printed))
             (flush-output)
             (define sleep-time
               (cond [(= i (- sides 2))
                      (define multiplier (take-random (list 2 4 8 12)))
                      (* multiplier base-sleep-time)
                      ]
                     [(= i (- sides 1))
                      (define multiplier (take-random (list 4 8 12 16)))
                      (* multiplier base-sleep-time)
                      ]
                     [else
                      (define multiplier (take-random (list 2 4 8)))
                      (* multiplier base-sleep-time)
                      ]))
             (sleep sleep-time)
             ))
         (for ([i bonus])
           ; TODO: display + for positive, - for negative bonuses
           (if (= i 0)
               (display ":")
               (display "."))
           (set! total-printed (add1 total-printed))
           (flush-output)
           (define sleep-time (* 2 base-sleep-time))
           (sleep sleep-time)
           )

         (cond
           [(all-fulfill-predicate?
             results
             (λ (r) (= r sides)))
            (when (not (null? on-crit-success)) (on-crit-success))]
           [(all-fulfill-predicate?
             results
             (λ (r) (= r 1)))
            (when (not (null? on-crit-failure)) (on-crit-failure))]
           )
         (newline)

        ;  (notice (format "[~a]~a = [~a]" (string-append* (add-between (map number->string results) " + ")) bonus-string roll-total))

        ;  (wait-for-confirm)

         roll-total
         ]
        [else
         (dev-note (format "unknown formula ~a" formula))
         (error "unknown formula")])
)

; returns (U 'critical-success 'success 'narrow-success 'failure 'critical-failure)
(define (check formula #:title title #:target-number tn #:bonus [bonus '()])
  (notice (format "~a [~a >= ~a]" title formula tn))
  (wait-for-confirm)
  (cond [(string=? formula "2d6")
         (define n 2)
         (define sides 6)
         (define results (for/list ([i n])
           (d 1 sides)))
         (define bonus-string "")
         (cond [(null? bonus) (set! bonus 0)]
               [else
                (set! bonus-string
                      (if (< bonus 0)
                          (format "- ~a" (abs bonus))
                          (format "+ ~a" bonus)))])
         (define roll-total (+ (apply + results) bonus))
         (define check-outcome
           (cond
            [(all-fulfill-predicate?
              results
              (λ (r) (= r sides)))
             'critical-success]
            [(all-fulfill-predicate?
              results
              (λ (r) (= r 1)))
             'critical-failure]
            [(= roll-total tn) 'narrow-success]
            [(> roll-total tn) 'success]
            [else 'failure]
            ))
         (define success-string (format "~a" check-outcome))

         (for ([i tn])
          (display ".")
          (flush-output))
         (newline)

         (define total-printed 0)

         (define base-sleep-time 0.05)
         (sleep (* 2 base-sleep-time))
         (for ([dice results])
           (for ([i dice])
             (if (= i 0)
                 (display ":")
                 (display "."))
             (set! total-printed (add1 total-printed))
             (flush-output)
             (define sleep-time
               (cond [(>= total-printed tn)
                      (* 2 base-sleep-time)]
                     [(= i (- sides 2))
                      (define multiplier (take-random (list 2 4 8 12)))
                      (* multiplier base-sleep-time)
                      ]
                     [(= i (- sides 1))
                      (define multiplier (take-random (list 4 8 12 16)))
                      (* multiplier base-sleep-time)
                      ]
                     [else
                      (define multiplier (take-random (list 2 4 8)))
                      (* multiplier base-sleep-time)
                      ]))
             (sleep sleep-time)
             ))
         (for ([i bonus])
           (if (= i 0)
               (display ":")
               (display "."))
           (set! total-printed (add1 total-printed))
           (flush-output)
           (define sleep-time (* 2 base-sleep-time))
           (sleep sleep-time)
           )
         (newline)
         (newline)

         (notice (format "[~a]~a = [~a >= ~a] [~a]" (string-append* (add-between (map number->string results) " + ")) bonus-string roll-total tn success-string))

         (wait-for-confirm)

         check-outcome
         ]
        [else
         (dev-note (format "unknown formula ~a" formula))
         (error "unknown formula")]))
