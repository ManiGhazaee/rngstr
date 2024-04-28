# rngstr

A command-line tool that generates random strings of characters with options to customize the character set or length of the generated string.

## Installation

```bash 
cargo install rngstr
```

## Usage 

```bash
rngstr [OPTIONS]
```

- Default character set = (A-Z, a-z, 0-9)
- Default length = 32
- Default copy to clipboard = true
- Default print = true

## Options
- `-l, --length <LENGTH>`:
    [default: 32]

- `-p, --password`:
    Use the password character set (A-Z, a-z, 0-9, and special characters)

- `-u, --url`:
    Use the URL character set (A-Z, a-z, 0-9, and -_.~)

- `-c, --custom <CUSTOM>`:
    Specify a string of custom characters (e.g. abcd0123)

- `--no-copy`:
    Don't copy the generated result to clipboard

- `--no-print`:
    Don't print the generated result

- `-h, --help`:
    Print help (see a summary with '-h')

- `-V, --version`:
    Print version