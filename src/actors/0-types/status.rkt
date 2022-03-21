#lang at-exp typed/racket

(provide (struct-out status))

(struct status
  ([type : Symbol]
   [lifetime : Natural])
  #:prefab
  #:mutable)

;; Statuses are semi-temporary.
;; They are often combat-related.
