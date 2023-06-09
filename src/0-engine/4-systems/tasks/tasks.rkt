#lang at-exp racket

(provide (all-defined-out))

(require "../../1-index/state.rkt"

         "../../2-core/io.rkt"
         "../../2-core/core.rkt"

         "../../3-types/task.rkt")

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (rules-to-lambda rules)
  `(λ () ,@rules))

(define (add-task! task)
  (current-tasks (append-element (current-tasks) task)))

; (-> Symbol (Maybe Task))
(define (task-exists? id)
  (define tasks (current-tasks))
  (if (null? tasks) #f (findf (λ (task) (equal? id (task-id task))) tasks)))

(define (complete-task! id)
  (define t (task-exists? id))
  (cond
    [(not (null? t))

     (when (not (null (task-on-complete! t)))
       ((rules-to-lambda (task-on-complete! t))))
     (set-task-state! t 'completed)]

    [else '()]))

(define (fail-task! id)
  (define t (task-exists? id))
  (cond
    [(not (null? t))

     (when (not (null (task-on-fail! t)))
       ((rules-to-lambda (task-on-fail! t))))
     (set-task-state! t 'failed)]

    [else '()]))

(define (add-new-task! task)
  (when (not (task-exists? (task-id task)))
    (add-task! task))

  (define body
    (list (tr (~a (task-name task)) (~a (task-info-blurb task)) (~a (task-status-text task)))))
  (info-card body "New task")
  (wait-for-confirm)
  '())

(define (create-task id)
  (define t
    (case id
      ['shaman-bag
       (task 'shaman-bag "Shaman bag" 'in-progress "Should be in the tunnels" "" '() '() '())]
      ['the-debt
       (task 'the-debt
             "The debt"
             (partially-completed 3 100)
             "Due on day 15"
             "100 grams of gold (3 g paid)"
             '()
             '()
             '())]

      ['evolve (task 'evolve "Evolve." 'in-progress "" "" '() '() '())]

      ['the-message
       (task 'the-message
             "Gas station attendant's message"
             'in-progress
             "Deliver to termites"
             "'Hartmann device is in the workshop'."
             '()
             '()
             '())]

      ['book-of-recombination
       (task 'find-book-of-recombination
             "Book of Recombination"
             'in-progress
             ""
             "The book on transcending death, should be in the Maw of Martaanvuo."
             '()
             '()
             'study-book-of-recombination)]

      ['hartmann-device
       (task
        'hartmann-device
        "Hartmann Device"
        'in-progress
        '"\"Null-State Collapse Initiator, the Last Key to the Last Lock, World-Unfurler, Pain-Ender.\""
        "Find it. Use it. ")] ; Or destroy it... in this timeline only

      [else (error (format "Unknown task id: ~a" id))]))
  (when (not (task-exists? id))
    (add-task! t))

  (define body (list (tr (~a (task-name t)) #;(~a (task-info-blurb t)) (~a (task-status-text t)))))
  (info-card body "New task")
  (wait-for-confirm))
