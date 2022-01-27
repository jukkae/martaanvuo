#lang racket

(require "../core/io.rkt"
         "../core/utils.rkt"
         "../resolvers/life-resolver.rkt"
         "../round-resolver/round-resolver.rkt"
         "../state/state.rkt")

(provide resolve-game)
(define (resolve-game game-mode)
  (case game-mode
    ['begin
     (br)
     (prln "Progress is saved automatically.")]

    ['restart
     '()]

    ['continue
     (define input-file (open-input-file "save.txt"))
     (define serialized-state (read input-file))

     (with-handlers ([exn:fail:contract:arity?
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
      (let begin-new-life ([mode (case game-mode
                                   ['begin 'begin]
                                   ['restart 'begin]
                                   ['continue 'continue])])

        (define pc-life-end-status (resolve-life mode))

        (when (eq? pc-life-end-status 'pc-dead)
          (let end-of-life-menu ([verbosity 'verbose])

            (define meta-commands (make-hash))
            (hash-set! meta-commands "Q" (cons "[Q]: Quit." quit))
            (hash-set! meta-commands "P" (cons "[P]: Proceed."
                                               (thunk (begin-new-life 'begin))))

            (p "Otava is dead. Proceed?")
            (print-meta-commands-with-keys meta-commands)
            (define input (wait-for-input))

            (newline)

            (cond ((meta-command-valid? meta-commands input)
                   (handle-meta-command meta-commands input)
                   (end-of-life-menu 'verbose))
                  (else (end-of-life-menu 'abbreviated)))))

        (when (eq? pc-life-end-status 'win-game) (end-game 'win-game))
        (when (eq? pc-life-end-status 'restart) (end-game 'restart)))))

  (case end-game-status

    ['win-game
     
     (wait-for-confirm)
     (p
      (string-append "The end."
                     "\n\n\n"
                     "M A R T A A N V U O"
                     "\n"
                     "==================="
                     "\n\n"
                     "Jukka Eerikäinen (2021)"
                     "\n\n"))
     (player-info)
     (wait-for-confirm)
     (exit)]
    
    ['restart

     (narrate-restart)

     (delete-save-file)
     (reset-situation!)

     (prln "Progress deleted. Starting from the beginning.")
     (resolve-game 'restart)]))

(define (narrate-restart)
  (p
   (take-random ; TODO: probabilitify these kinds of take-random calls
    (list
     "In the depths of the Maw, the Heart of the World stops. Then, it's all black."
     (string-append "The end."
                    "\n\n\n"
                    "M A R T A A N V U O"
                    "\n"
                    "==================="
                    "\n\n"
                    "Jukka Eerikäinen (2021)"
                    "\n\n")
     "[details omitted – 3 days later] Having passed Martaanvuo, Otava comes upon an unnamed mountain range. She crosses over and begins a new life herding reindeer. She lives the rest of her days free from suffering and dies of natural causes at an elderly age."))))

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
         (prln "It was [D] or [Q], but nevermind. Your saved progress was not deleted, but it is still corrupt or incompatible.")
         (br)
         (prln "Martaanvuo awaits your return.")
         (exit))))

(define (on-begin-playthrough!)
  (reset-situation!))
