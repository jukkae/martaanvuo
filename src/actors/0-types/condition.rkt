#lang at-exp typed/racket

(provide (struct-out condition))

(require "../../core/maybe.rkt")

(struct condition
 ([type : Symbol]
  [details : (U String (Listof (U Symbol String Number)))]
  [on-end-round-rules : (Maybe Sexp)])
  #:prefab
  #:mutable)

;; Conditions are semi-permanent.

; Wounds: Move from state to state
; Fresh wound [acquired-at]
; -> Treated wound [treated-at time-until-healed] (well treated / poorly treated; healing / not healing)
; -> Healed wound
; -> Poorly healed wound -> possible permanent condition = negative modifiers
