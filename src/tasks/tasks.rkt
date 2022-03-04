#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

; TODO: This should reprovide task and act as 'module entrypoint'
(require "task.rkt"
         "../core/io.rkt"
         "../core/utils.rkt")

(lazy-require ["../state/state.rkt"
               (current-tasks)])

(define (add-task! task)
  (current-tasks
   (append-element (current-tasks) task)))

; (-> Symbol (Maybe Task))
(define (task-exists? id)
  (define tasks (current-tasks))
  (if (null? tasks)
      #f
      (findf (λ (task) (eq? id (task-id task))) tasks)))

(define (create-task id)
  (define t
    (case id
      ['pay-off-debt
       (task
        'pay-off-debt
        "Pay off debt"
        (partially-completed 0 10.111)
        "Long story."
        "10,111 grams of gold left."
        '()
        '()
        '()
        )]

      ['anthead-monograph
       (task
        'find-anthead-monograph
        "Anthead Monograph"
        'in-progress
        "Ancient book contains knowledge to become deathless."
        "Find it. Should be in the Maw (?) of Martaanvuo."
        '()
        '()
        'study-anthead-monograph
        )]

      ['grabberkin-fingers
        (task
        'grabberkin-fingers
        "Grabberkin fingers for Anthill"
        (partially-completed 0 5)
        "Cause and effect: 5 fingers -> 1 g gold."
        "In progress: 0"
        '()
        '()
        '()
        )]

      ['hartmann-device
       (task
        'hartmann-device
        "Hartmann Device"
        'in-progress'
        "\"Null-State Collapse Initiator, the Last Key to the Last Lock, World-Unfurler, Pain-Ender.\""
        "Find it. Use it. ")] ; Or destroy it... in this timeline only

      [else (error (format "Unknown task id: ~a" id))]))
  (when (not (task-exists? id))
    (add-task! t))

  (define body
    (list
     (tr
       (~a (task-name t))
       (~a (task-info-blurb t))
       (~a (task-status-text t)))))
  (info-card
   body
   "New task"
   )
  (wait-for-confirm)
)
#|

Magpie King's War
- Loot Cache
 a) magpie takes more
 b) you take more
- Free the Slaves (rescue; stealth, timing, or just combat)
- Get Weapons (from the Swamp)
- Loot Cache 2 (this time there's a fucking surprise there) (with Magpie's Hench) ("Hello, Otava. I am Magpie." A new voice greets Otava. "It – the previous one – is now King of Magpies, and as such, it is not proper for Its Kingliness to associate with the riff-raff. But I am quite sure we will learn to get along with each other in the future, as we go on innumerable adventures together.") -> I'm already loving it. / I'm already hating it.
- Break into Control Room (stealth?)
- Break into Reactor Room
  -> fight Aberration
  -> escape with a time limit
- Fish for Magpie

Rise of Anthill
- Grabberkin Fingers (ongoing, will buy lots; there's a bag somewhere in the Facility)
- Search for the Specific Cog
- Eliminate Aberration
- Acquire a Certain Memory Drive (from Control Room) (if not explicitly looking for it, then it will require a while to search – or just appear alongside junk)
- Fish for Anthill (this'll cause tensions)

Anthead Monograph
- Find It (in The Maw)
- Study It (prerequsite: Annexed Murkwater Files)
- Enter Waiting Room <- ending

Annexed Murkwater Files, Batch 1
- Find It (in The Facility)
- Study It
- Information about Aberration
- Information about More Gold

Annexed Murkwater Files, Batch 2
- Find It (in The Facility)
- Study It
- Information about Martaanvuo Source and Various Temporo-Ontological Fluxuations in the Environs
- Information about The Tunnel Between The Worlds

The Extremist Faction
- Destroy the World <- one actual ENDING

The Debt
- Pay It Off (NOTE: "countdown" thing)

"unsettled: 10,111 grams of gold" ; MAKE A FUCKING PLOT POINT OF THIS
              10111

Mieli
- I will meet you again
- Save me from the Collapse


- Defend the Hack (defense against invaders)

|#
