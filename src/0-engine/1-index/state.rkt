#lang at-exp racket

(require racket/lazy-require)

(provide append-to-log
         next-chapter!

         current-chapter
         current-combat-timeline
         current-counters
         current-counters++
         current-elapsed-time
         current-fragment-id
         current-in-combat?
         current-last-numeric-actor-id
         current-last-numeric-actor-id++
         current-last-paragraph
         current-location
         current-log
         current-part
         current-pending-action
         current-prompt
         current-tasks
         current-epithets
         current-world

         get-current-enemies
         get-current-enemy
         get-next-numeric-actor-id

         clear-current-fragment!

         once-per-day-action-done?
         mark-once-per-day-action-done!

         reset-pending-action!

         set-flag
         remove-flag
         flag-set?
         toggle-flag)

(lazy-require ["../7-state/logging.rkt" (append-to-log next-chapter!)])

(lazy-require ["../7-state/flags.rkt" (set-flag remove-flag flag-set? toggle-flag)])

(lazy-require ["../7-state/state.rkt"
               (current-chapter current-combat-timeline
                                current-counters
                                current-counters++
                                current-elapsed-time
                                current-fragment-id
                                current-in-combat?
                                current-last-numeric-actor-id
                                current-last-numeric-actor-id++
                                current-last-paragraph
                                current-location
                                current-log
                                current-part
                                current-pending-action
                                current-prompt
                                current-tasks
                                current-epithets
                                current-world
                                get-current-enemies
                                get-current-enemy
                                get-next-numeric-actor-id
                                pc
                                clear-current-fragment!
                                once-per-day-action-done?
                                mark-once-per-day-action-done!
                                reset-pending-action!)])
