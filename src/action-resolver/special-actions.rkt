#lang racket

(provide resolve-special-action!)

(require racket/lazy-require)

(require "../action.rkt"
         "../io.rkt"
         "../utils.rkt")

(lazy-require
 ["../state/mutators.rkt"
  (flag-set?
   set-flag
   )])

(lazy-require
 ["../state/logging.rkt"
  (next-chapter!
   )])

(define (resolve-special-action! action)
  (case (action-symbol action)
    ['end-run
     (cond ((flag-set? 'ending-run-allowed)
            #;(p "At least it's something.")
            'end-run)
           (else
            (set-flag 'tried-to-go-back)
            (p "The unexpected fork is worrisome. Otava must have taken the wrong turn somewhere. She decides to turn back, make sure she hasn't missed anything.")
            (wait-for-confirm)
            (next-chapter!) ; end chapter, but not run!
            (p "Otava is getting close to what she's looking for, but she has trouble remembering how she got here. Did she follow the trail of the Broker? Yes, yes she did. What was she doing here?")
            (wait-for-confirm)
            (p "The Facility. She is looking for the Facility at Martaanvuo, to pay back her debt to the Collector. Broker's trail comes to a fork.")
            (p "To the left, the trail turns into a climb up a rocky hill. A magpie's call echoes from somewhere up the hill. An army of ants is marching down the other branch, toward what must be Martaanvuo swamp.")
            'failure
            ))]
    
    ['back-off 'ok]
    ['win-game 'win-game]
    ['skip(cond ((member 'silent (action-details action))
              'ok)
             (else
              'ok))]
    [else (dev-note "unknown special action:")
          (displayln action)])
  
  )