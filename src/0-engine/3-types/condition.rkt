#lang at-exp typed/racket

(provide (struct-out condition)
         (struct-out Illness)
         (struct-out FreshInjury))

(require "../2-core/maybe.rkt")

; TODO: hide acquired-at from callers
(struct condition
        ([type : Symbol] [acquired-at : Natural] ; TODO: more precisely, timestamp
                         [details : (U String (Listof (U Symbol String Number)))])
  #:prefab
  #:mutable)

(define-type IllnessState (U 'incubating 'acute 'postacute))

(struct Illness condition ([state : IllnessState]) #:prefab #:mutable)

(define-type Injury (U FreshInjury TreatedInjury HealedInjury))

(struct FreshInjury condition ([acquired-at : Natural]) #:prefab #:mutable)

(define-type TreatmentSuccess (U 'poorly-treated 'well-treated))

(struct TreatedInjury
        condition
        ([treated-at : Natural] [time-until-healed : Natural]
                                [healing? : Boolean]
                                [treatment-success : TreatmentSuccess])
  #:prefab
  #:mutable)

(define-type HealedOutcome (U 'healed-well 'healed-poorly))

(struct HealedInjury condition ([healed-at : Natural] [healed-outcome : HealedOutcome])
  #:prefab
  #:mutable)
