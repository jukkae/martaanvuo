#lang typed/racket

(provide (all-defined-out))

(require "../../../core/maybe.rkt"
         "event.rkt")

(struct timeline
 ([metadata : (Listof Symbol)]
  [events : (Listof event)]
  [duration : Natural])
  #:prefab
  #:mutable)
