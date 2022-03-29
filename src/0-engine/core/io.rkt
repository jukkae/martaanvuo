#lang at-exp racket

(provide (all-defined-out))
(provide (all-from-out "output.rkt"))
(require "output.rkt")

(provide (all-from-out "input.rkt"))
(require "input.rkt")

(require racket/lazy-require)
(require text-table)

(require "../2-core/list-utils.rkt")

(lazy-require
 ["../state/state.rkt"
  (current-part
   current-chapter
   current-log
   current-last-paragraph
   current-prompt)])

(lazy-require
 ["../state/logging.rkt"
  (append-to-log)])


(define (write-save-file serialized-state)
  (define output-file (open-output-file "save.txt" #:exists 'truncate)) ; truncate = delete if exists
  (write serialized-state output-file)
  (close-output-port output-file))

(define (delete-save-file)
  (delete-file "save.txt"))
