#lang at-exp racket

(require
  "../1-life-resolver/life-resolver.rkt"
  "../3-round-resolver/round-resolver.rkt"

  "../../2-core/io.rkt"
  "../../2-core/core.rkt"

  "../../4-rules/blurbs/blurbs.rkt"

  "../../7-state/state/state.rkt"
  "../../7-state/save-game/save-game.rkt"
  )

(provide resolve-game)
(define (resolve-game game-mode)

  (when (vector-member "--restart" (current-command-line-arguments))
    (if (file-exists? "save.txt")
        (begin
          (notice "--restart flag supplied, deleting save file.")
          (delete-save-file))
        (begin
          (notice "--restart flag supplied, but no save exists.")))
    (set! game-mode 'begin))

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
     (blurb 'the-end)
     (wait-for-confirm)

     (display-playthrough-stats)
     (wait-for-confirm)

     (exit)]

    ['restart
     (narrate-restart)

     (display-playthrough-stats)
     (wait-for-confirm)

     (reset-situation!)
     (delete-save-file)
     (prln "Progress deleted. Starting from the beginning.")
     (newline)
     (wait-for-confirm)

     (resolve-game 'restart)]))


(define (display-playthrough-stats)
  (define body
    (tbody
      (tr "MARTAANVUO")
      (tr "")
      (tr "Otava did not become deathless.")
      (tr "Otava did not pay back her debt.")
      (tr "Otava did not escape Martaanvuo.")
      (tr "The world continues to exist.")
      (tr "")
      (tr (format "Otava ran ~a ~a and lived ~a ~a."
                  (current-run)
                  (if (= (current-run) 1) "run" "runs")
                  (current-life)
                  (if (= (current-life) 1) "life" "lives")
                  ))
      ))
  (newline)
  (info-card body '()))

(define (narrate-restart)
  (p
   (take-random
    (list
     "An unfinished timeline continues its chaotic growth, an eldritch tentacle tying itself in a thousand knots through dimensions of time and space."
     "Unpruned, abandoned, the story ends here."
     "Unpruned, abandoned, the story of Otava ends here."
     "Unpruned, abandoned, the story of this Otava ends here."
     "Unpruned, abandoned, this story of this Otava ends here."
     "In the depths of the Maw, the Heart of the World stops."
     (get-blurb 'the-end)
     "[data missing - 13 days later] Otava comes upon a mountain range. She finds a way over, and begins a new life on the other side, deep within the vast unnamed forests.")
    #:distribution 'quadratic)))

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
