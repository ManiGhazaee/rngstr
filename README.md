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

### Defaults:

- character set = (A-Z, a-z, 0-9)
- length = 32
- copy to clipboard = true
- print = true
- suffix = ""
- prefix = ""
- repeat = 1

## Options
- `-l, --length <LENGTH>`:

- `-c, --custom <CUSTOM>`:
          Specify a string of custom characters (e.g. `abc01111`)

- `--regex <REGEX>`:
          Specify a regular expression pattern to be used to generate the character set (e.g. `[0-9A-F]` will generate `0123456789ABCDEF` character set)

- `-p, --prefix <PREFIX>`:
          Specify a string to be prepended to the generated string

- `-s, --suffix <SUFFIX>`:
          Specify a string to be appended to the generated string

- `-r, --repeat <REPEAT>`:
          Specify number of times string should be generated

- `--no-copy`:
          Don't copy the generated result to clipboard

- `--no-print`:
          Don't print the generated result

- `--password`:
          Use the password character set (A-Z, a-z, 0-9, and special characters)

- `-h, --help`:
          Print help (see a summary with '-h')

- `-V, --version`:
          Print version