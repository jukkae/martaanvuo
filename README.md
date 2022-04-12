# Martaanvuo

## Requirements

- Download https://download.racket-lang.org/ or install cli Racket (`brew install racket`)
- Install Racket packages: `text-table`, `rebellion`, and `reprovide-lang-lib` (if not installed). (`raco pkg install text-table` for cli racket)
- VS Code might require `racket-langserver`.

## Development

To update packages, `raco pkg update --all`.

## Cross-compilation for Windows

NB: Tested on x86_64 Linux, Racket 8.4.

For cross-compilation, `raco pkg install raco-cross` is needed.

Then, `raco cross --target x86_64-win pkg install at-exp-lib text-table rebellion reprovide-lang-lib`.

Then, to compile, run the prepare and cross-compile tasks.

Clean compiled files between swithing build platforms (there's a VSC task for it). Compiled files  cause `fasl-read: incompatible fasl-object machine-type 'ta6nt found in #<binary input port bytevector>` issue.
