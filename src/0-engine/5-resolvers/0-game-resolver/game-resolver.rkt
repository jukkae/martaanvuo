#lang at-exp racket

(require
  "../1-round-resolver/round-resolver.rkt"
  "../1-round-resolver/get-next-pc-action/menu.rkt"
  "../1-round-resolver/get-next-pc-action/ui.rkt"

    "../../1-index/content.rkt"

  "../../2-core/io.rkt"
  "../../2-core/core.rkt"
  "../../2-core/save-game.rkt"

  "../../4-systems/blurbs/blurbs.rkt"

  "../../7-state/state.rkt"

  "../../../1-content/narration/game-resolver.rkt"
  )

(provide resolve-game)
(define (resolve-game game-mode)

  (define seed-flag '())

  (command-line
   #:once-each
   [("-s" "--seed") seed "Set random seed for new game"
    (seed-rng! (exact-floor (string->number seed)))
    (set! seed-flag (exact-floor (string->number seed)))]
   [("-r" "--restart") "Restart (deletes old game)"
    (if (file-exists? "save.txt")
        (begin
          (notice "--restart flag supplied, deleting save file.")
          (delete-save-file))
        (begin
          (notice "--restart flag supplied, but there is no previous save file.")))
    (set! game-mode 'begin)])

  (case game-mode
    ['begin
 (br)
 (prln "Progress is saved automatically.")
 (if (not (null? seed-flag))
     (seed-rng! seed-flag)
     (reset-rng!))
 ]

    ['restart
     (if (not (null? seed-flag))
         (seed-rng! seed-flag)
         (reset-rng!))
     ]

    ['continue
     (define input-file (open-input-file "save.txt"))
     (define serialized-state (read input-file))

     (with-handlers ([exn:fail:contract:arity?
                      (λ (exn)
                        (handle-broken-save)
                        (set! game-mode 'begin))]
                     [exn:fail:contract?
                      (λ (exn)
                        (handle-broken-save)
                        (set! game-mode 'begin))])
       (load-situation-from-state serialized-state)
       (br)
       (prln "Progress loaded."))])

  (title)

  (case game-mode
    ['begin   (on-begin-playthrough!)]
    ['restart (on-begin-playthrough!)]
    ['continue
     (for ([entry (current-log)])
       (print-paragraph (format-for-printing entry #:width 84 #:indent 4)))
     (hr)])

  (define end-game-status
    (let/ec end-game
      (let loop ()
        (define round-exit-status (resolve-round 'begin))
        (case round-exit-status
          ['pc-dead
           (let end-of-life-menu ([verbosity 'verbose])

            (define meta-commands (make-hash))
            (hash-set! meta-commands "Q" (cons "[Q]: Quit." quit))
            (hash-set! meta-commands "P" (cons "[P]: Proceed."
                                               (thunk
                                                 (on-begin-life)
                                                 (on-begin-run)
                                                 'ok)))

            (p "Otava is dead. Proceed?")
            (print-meta-commands-with-keys meta-commands)
            (define input (wait-for-input))

            (newline)

            (cond ((meta-command-valid? meta-commands input)
                   (handle-meta-command meta-commands input))
                  (else (end-of-life-menu 'abbreviated))))
           ]
          ['win-game
           (end-game 'win-game)]
          ['restart
           (end-game 'restart)]
          ['ok '()]
          [else
           (dev-note (format "Unexpected round exit status: ~a" round-exit-status))]
          )

        (loop))))


  (case end-game-status

    ['win-game
     (blurb 'the-end)
     (wait-for-confirm)

     (display-playthrough-stats)
     (wait-for-confirm)

     (exit)]

    ['restart
     (narrate-restart)

     (display-playthrough-stats)
     (wait-for-confirm)

     (reset-world!)
     (delete-save-file)
     (prln "Progress deleted. Starting from the beginning.")
     (newline)
     (wait-for-confirm)

     (resolve-game 'restart)]))

(define (handle-broken-save)
  (br)
  (prln "Save file is corrupt or incompatible with this revision of Martaanvuo. Delete saved progress? [D] to delete, [Q] to quit without deleting.")

  (define input (wait-for-input))
  (set! input (string-upcase input))

  (cond ((equal? input "D")
         (delete-save-file)
         (br)
         (prln "Progress deleted. Starting from the beginning. Progress is saved automatically."))

        ((equal? input "Q")
         (br)
         (prln "Saved progress was not deleted, but it is still corrupt or incompatible.")
         (br)
         (prln "Come back soon. Martaanvuo is eager for your return.")
         (exit))

        (else
         (br)
         (prln "Your saved progress was not deleted, but it is still corrupt or incompatible.")
         (br)
         (prln "Martaanvuo awaits your return.")
         (exit))))

(define (on-begin-playthrough!)
  (reset-world!)
  ;TODO: Move these somewhere
  (on-begin-life)
  (on-begin-run))
