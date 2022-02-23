#lang at-exp typed/racket

(provide (struct-out condition))

(require "../../core/maybe.rkt")

(struct condition
 ([type : Symbol]
  [details : (Listof (U Symbol String))]
  [on-end-round-rules : (Maybe Sexp)])
  #:prefab
  #:mutable)


