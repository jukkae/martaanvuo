#lang racket

(require "io.rkt")
(require "life-resolver.rkt")
(require "round-resolver.rkt")
(require "situation.rkt")
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
  ;(paragraph "[" "Begin a story" "]")
  (setup-world)
  )


(define (resolve-game mode)
  (case mode
    ['begin
     (newline)
     (displayln "Progress is saved automatically.")]
    
    ['continue
     (define input-file (open-input-file "save.txt"))
     (define situation (read input-file))
     (load-situation situation)
     (newline)
     (displayln "Progress loaded.")])
  
  (title)

  (case mode
    ['begin (on-begin-playthrough)]
    
    ['continue
     (for ([entry (get-log)])
       (print-paragraph (format-for-printing entry #:width 84 #:indent 4)))
     (hr)])

  (define end-game-status
    (let/ec end-game
      (let begin-new-life ()
        (define pc-life-end-status (resolve-life mode))
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

            (paragraph "Proceed?")
            (print-meta-commands-with-keys meta-commands)
            (define input (wait-for-input))
            (serialize-input)

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
     (displayln "Progress deleted. Restarting.")
     (resolve-game 'begin)]))

; engine / game-resolver?
(define (begin-session)
  #; (random-seed 13)

  (if (file-exists? "save.txt")
      (resolve-game 'continue)
      (resolve-game 'begin)))

; main entrypoint
(begin-session)