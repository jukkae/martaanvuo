#lang typed/racket

(provide (struct-out task)
         TaskState
         (struct-out partially-completed))

(require "../2-core/maybe.rkt")

; x out of y
(struct partially-completed
  ([x : Number]
   [y : Number])
  #:prefab
  #:mutable)

(define-type TaskState (U 'pending 'in-progress partially-completed 'completed 'failed))

(struct task
  ([id : Symbol]
   [name : String]
   [state : TaskState]
   [info-blurb : String]
   [status-text : String]
   [on-complete! : (Maybe Sexp)]
   [on-fail! : (Maybe Sexp)]
   [next-task-id : (Maybe Symbol)]
   )
  #:prefab
  #:mutable)
