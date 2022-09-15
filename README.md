# Martaanvuo

## Requirements

- Download https://download.racket-lang.org/ or install cli Racket (`brew install racket`)
- Install Racket packages: `text-table`, `rebellion`, `reprovide-lang-lib`, `rackjure`, `anaphoric`,  and `mred-designer` (if not installed). (`raco pkg install --auto text-table rebellion reprovide-lang-lib rackjure anaphoric mred-designer`
- VS Code might require `racket-langserver`.

## Development

To update packages, `raco pkg update --all`.

## Recommended packages for DrRacket

- `files-viewer`

## Cross-compilation for Windows

NB: Tested on x86_64 Linux, Racket 8.4.

For cross-compilation, `raco pkg install raco-cross` is needed.

Then, `raco cross --target x86_64-win pkg install at-exp-lib text-table rebellion reprovide-lang-lib`.

Then, to compile, run the prepare and cross-compile tasks.

Clean compiled files between swithing build platforms (there's a VSC task for it). Compiled files  cause `fasl-read: incompatible fasl-object machine-type 'ta6nt found in #<binary input port bytevector>` issue.
