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

Then, `raco cross --target x86_64-win make src/martaanvuo`.

Then, to compile, `raco cross --target x86_64-win exe --orig-exe --embed-dlls -o build/martaanvuo.exe src/martaanvuo.rkt`.
