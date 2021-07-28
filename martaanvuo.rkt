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
  (define title-string
    (string-append "M A R T A A N V U O"
                   "\n"
                   "==================="))
  (paragraph title-string))


; engine / game-resolver? meta player.rkt?
(define (on-begin-playthrough)
  ;(paragraph "[" "Begin a story" "]")
  (setup-world)
  )

(define (load-save-file)
  (define input-file (open-input-file "save.txt"))
  (define situation (read input-file))
  (load-situation situation)
  (newline)
  
  (title)

  (let/ec win-game
    (let continue-life ()
      (define pc-life-end-status (continue-a-life))
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
          (hash-set! meta-commands "P" (cons "[P]: Proceed." begin-new-game))

          (paragraph "Proceed?")
          (print-meta-commands-with-keys meta-commands)
          (define input (wait-for-input))
          (serialize-input)

          (newline)

          (cond ((meta-command-valid? meta-commands input) (handle-meta-command meta-commands input))
                (else (end-of-life-menu 'abbreviated)))))
      (when (eq? pc-life-end-status 'win-game) (win-game))))
  (end-game)
  )

(define (begin-new-game)
  (displayln "beginning new game")
  (title)
  
  (on-begin-playthrough)
  (let/ec win-game
    (let begin-new-life ()
      (define pc-life-end-status (resolve-a-life))
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
      (when (eq? pc-life-end-status 'win-game) (win-game))))
  (end-game))

; engine / game-resolver?
(define (begin-game)
  #; (random-seed 13)

  (if (file-exists? "save.txt")
      (load-save-file)
      (begin-new-game)))

; main entrypoint
(begin-game)