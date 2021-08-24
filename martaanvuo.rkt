#lang racket

(require "fragment.rkt")
(require "fragments.rkt") ; has to be required somewhere!
(require "io.rkt")
(require "life-resolver.rkt")
(require "round-resolver/round-resolver.rkt")
(require "state/state.rkt")
(require "utils.rkt")
(require "world.rkt")



; game-specific, not engine!
(define (title)
  (newline)
  ;(displayln "--------------------------------------------------------------------------------") ; 80
  (displayln "--------------------------------------------------------------------------------------------") ; 92
  (newline)
  (display-title))


; engine / game-resolver? meta player.rkt?
(define (on-begin-playthrough)
  (reset-situation!)
  (setup-world)
  )


(define (handle-broken-save)
  (newline)
  (displayln "Save file is corrupt or incompatible with this revision of Martaanvuo. Delete saved progress? [D] to delete, [Q] to quit without deleting.")
  
  (define input (wait-for-input))
  (set! input (string-upcase input))
  
  (cond ((equal? input "D")
         (delete-save-file)
         (newline)
         (displayln "Progress deleted. Starting from the beginning. Progress is saved automatically."))

        ((equal? input "Q")
         (newline)
         (displayln "Saved progress was not deleted, but it is still corrupt or incompatible.")
         (newline)
         (displayln "Come back soon. Martaanvuo is eager for your return.")
         (exit))
          
        (else
         (newline)
         (displayln "It was [D] or [Q], but nevermind. Your saved progress was not deleted, but it is still corrupt or incompatible.")
         (newline)
         (displayln "Martaanvuo awaits your return.")
         (exit))))


(define (resolve-game mode)
  (case mode
    ['begin
     (newline)
     (displayln "Progress is saved automatically.")]

    ['restart '()] ; If the player restarts, they should know that progress is saved automatically.
    
    ['continue
     (define input-file (open-input-file "save.txt"))
     (define serialized-state (read input-file))
     
     (with-handlers ([exn:fail:contract:arity?
                      (λ (exn)
                        (handle-broken-save)
                        (set! mode 'begin))])
       (load-situation-from-state serialized-state)
       (newline)
       (displayln "Progress loaded."))])
  
  (title)

  (case mode
    ['begin (on-begin-playthrough)]
    ['restart (on-begin-playthrough)]
    
    ['continue
     (for ([entry (current-log)])
       (print-paragraph (format-for-printing entry #:width 84 #:indent 4)))
     (hr)])

  (define life-resolver-mode
    (case mode
      ['begin 'begin]
      ['restart 'begin]
      ['continue 'continue]))

  (define end-game-status
    (let/ec end-game
      (let begin-new-life ()
        (define pc-life-end-status (resolve-life life-resolver-mode))
        (when (eq? pc-life-end-status 'pc-dead)

          (let end-of-life-menu ([verbosity 'verbose])
            (define (handle-meta-command meta-commands-with-keys input)
              (set! input (string-upcase input))
              (define meta-command-with-key (hash-ref meta-commands-with-keys input '()))
              (define meta-command (cdr meta-command-with-key))
              (meta-command)
              (end-of-life-menu 'verbose))

            (define meta-commands (make-hash))
            (hash-set! meta-commands "Q" (cons "[Q]: Quit." quit))
            (hash-set! meta-commands "P" (cons "[P]: Proceed." begin-new-life))

            (p "Proceed?")
            (print-meta-commands-with-keys meta-commands)
            (define input (wait-for-input))
            
            (newline)

            (cond ((meta-command-valid? meta-commands input) (handle-meta-command meta-commands input))
                  (else (end-of-life-menu 'abbreviated)))))
        (when (eq? pc-life-end-status 'win-game) (end-game 'win-game))
        (when (eq? pc-life-end-status 'restart) (end-game 'restart)))))
  
  (case end-game-status
    ['win-game (end-game)]
    ['restart
     (delete-save-file)
     (reset-situation!)
     (displayln "Progress deleted. Starting from the beginning. Progress is saved automatically.")
     (resolve-game 'restart)]))

; engine / game-resolver?
(define (begin-session)
  #; (random-seed 13)

  (if (file-exists? "save.txt")
      (resolve-game 'continue)
      (resolve-game 'begin)))

; main entrypoint
(begin-session)