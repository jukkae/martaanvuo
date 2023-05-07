# Martaanvuo

Martaanvuo is a Master of Arts thesis game for Aalto University Media lab. It is a text-based body horror role-playing game that explores change, identity, and the nature of consciousness.

## Development requirements

Martaanvuo is written in Racket to maximize structural flexibility.

- Download https://download.racket-lang.org/ or install cli Racket (`brew install racket`)
- Install Racket packages: `text-table`, `rebellion`, `reprovide-lang-lib`, `rackjure`, `anaphoric`, `fmt`,  and `mred-designer`: `raco pkg install --auto text-table rebellion reprovide-lang-lib rackjure anaphoric fmt mred-designer`. When installing new packages, also install them for cross-compilation: `raco cross --target --x86_64-win pkg install package-name`.
- VS Code might require `racket-langserver`.

## Profiling

To run profiler, start the game with `raco profile src/martaanvuo.rkt` and press `Ctrl + C` to end profiling.

## Recommended packages for DrRacket

- `files-viewer`

## Cross-compilation for Windows

For cross-compilation, `raco pkg install raco-cross` is needed.

Then, install packages with `raco cross --target x86_64-win pkg install at-exp-lib text-table rebellion reprovide-lang-lib rackjure anaphoric`.

Then, to compile, run the prepare and cross-compile tasks.

Clean compiled files between swithing build platforms (there's a VSC task for it). Compiled files  cause `fasl-read: incompatible fasl-object machine-type 'ta6nt found in #<binary input port bytevector>` issue.
