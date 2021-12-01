#lang racket

(require "fragments/fragments.rkt") ; fragments have to be required somewhere!

(require "core/io.rkt"
         "resolvers/life-resolver.rkt"
         "round-resolver/round-resolver.rkt"
         "state/state.rkt")

; engine / game-resolver? meta player.rkt?
(define (on-begin-playthrough!)
  (reset-situation!))



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
    ['win-game (end-game)]
    ['restart
     (delete-save-file)
     (reset-situation!)

     (prln "Progress deleted. Starting from the beginning.")
     (resolve-game 'restart)]))

(define (handle-meta-command meta-commands-with-keys input)
              (set! input (string-upcase input))
              (define meta-command-with-key (hash-ref meta-commands-with-keys input '()))
              (define meta-command (cdr meta-command-with-key))
              (meta-command))


(define (begin-session)
  ; (random-seed 13)

  (if (file-exists? "save.txt")
      (resolve-game 'continue)
      (resolve-game 'begin)))

(begin-session)
