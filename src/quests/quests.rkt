#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "quest.rkt"
         "../core/io.rkt")

(lazy-require
 ["../state/state.rkt"
  (add-quest!
   quest-exists?)])

(define (create-quest quest-symbol)
  (define q
    (case quest-symbol
      ['pay-off-debt
       (quest 'pay-off-debt
              "Debt to Mediator"
              "in progress"
              "unsettled: 10,111 grams of gold" ; MAKE A FUCKING PLOT POINT OF THIS
              10111)]
      ['anthead-monograph
       (quest 'anthead-monograph
              "Anthead Monograph"
              "in progress"
              "find the Maw, find the Book"
              '())]
      ['loot-the-cache
       (quest 'loot-the-cache
              "Loot the Cache"
              "in progress"
              "power plant (?) on the hill"
              '())]
      ['grabberkin-finger
       (quest 'grabberkin-finger
              "Grabberkin finger"
              "in progress"
              "Anthill: 29 g gold / each"
              '())]))

  (when (not (quest-exists? quest-symbol))
    (add-quest! q)

    (info-card
     (tbody
      (tr (format "~a" (quest-title q))
          (format "~a" (quest-status q))
          (format "~a" (quest-notes q))))
     "New quest")

    (wait-for-confirm)))


#|
  (define-type task-state (U 'pending 'in-progress 'completed 'failed))

  (task
    id
    title
    requirements
    rewards
    failure-sanctions
    state
    )

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

The Rise of the Anthill
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
- Information about Martaanvuo Source and Various Temporo-Ontological Fluxuations in the Environs
- Information about The Tunnel Between The Worlds

The Extremist Faction
- Destroy the World <- one actual ENDING

The Debt
- Pay It Off

Mieli
- I will meet you again
- Save me from the Collapse


- Defend the Hack (defense against invaders)

|#