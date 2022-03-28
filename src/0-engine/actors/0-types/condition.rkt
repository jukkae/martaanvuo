#lang at-exp typed/racket

(provide
  (struct-out condition)
  (struct-out FreshInjury))

(require "../../core/maybe.rkt")

(struct condition
 ([type : Symbol]
  [details : (U String (Listof (U Symbol String Number)))])
  #:prefab
  #:mutable)


(define-type Injury (U FreshInjury TreatedInjury HealedInjury))


(struct FreshInjury condition
  ([acquired-at : Natural])
  #:prefab
  #:mutable)


(define-type TreatmentSuccess (U 'poorly-treated 'well-treated))

(struct TreatedInjury condition
  ([treated-at : Natural]
   [time-until-healed : Natural]
   [healing? : Boolean]
   [treatment-success : TreatmentSuccess])
  #:prefab
  #:mutable)


(define-type HealedOutcome (U 'healed-well 'healed-poorly))

(struct HealedInjury condition
  ([healed-at : Natural]
   [healed-outcome : HealedOutcome])
  #:prefab
  #:mutable)
