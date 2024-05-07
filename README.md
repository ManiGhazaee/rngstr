# rngstr

A cli tool for generating random strings of characters with customization options and a small domain specific language.

## Installation

```bash 
cargo install rngstr
```

## Usage 

```bash
rngstr [OPTIONS]
```

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

- `-d, --dsl <DSL>...`:
          Specify path of the source file as first argument and optional path of destination file as second argument (it will print to terminal if not specified) 

- `--no-copy`:
          Don't copy the generated result to clipboard

- `--no-print`:
          Don't print the generated result

- `--password`:
          Use the password character set (A-Z, a-z, 0-9, and special characters)

- `-t, --trailing-suffix`:
          Set trailing suffix generation to true

- `-h, --help`:
          Print help (see a summary with '-h')

- `-V, --version`:
          Print version

### Defaults:

By default, `rngstr` generates random strings with the following settings:

- character set = `[A-Za-z0-9]`
- length = `0`
- copy to clipboard = `true`
- print = `true`
- suffix = `""`
- prefix = `""`
- repeat = `1`

## Examples

```bash
rngstr --regex [0-9a-z] -l 8 -s ", " -r 3 
```
```bash
ln4hc1fk, srr9p704, ono7y09k 
```

---

```bash
rngstr --range 0..60 -s ", " -r 8 --trailing-suffix
```
```bash
6, 2, 5, 41, 33, 25, 3, 21,
```

---

```bash
rngstr -c 0001 -l 8 -p "0b" -s " " -r 3 
```
```bash
0b00000010 0b01100010 0b00001000
```

## DSL Usage

### Command declaration

```
!<name>: [OPTIONS]
```

- `!<name>`: This is the name of the command, which can be used to reference and call it later.
- `[OPTIONS]`: These are the options that can be specified for the command, following the same syntax as the command-line options for the rngstr tool.

### Command call

After the first non-command line (a trimmed line not starting with `!`), you can call the declared commands using the following syntax:

```
!<name>()
```

or

```
!<name>($)
```

where `$` is a placeholder for the generated string.

You can also use multiple placeholders within a command call, where each `$` will generate the same string.

```
!<name>($ $)
```

## DSL Examples

```bash
rngstr --dsl foo.txt bar.json 
```
foo.txt
```
!foo: --regex [0-9A-Z] -l 8 -s ",\n    "  -r 9
!bar: --range 0..99         -s ", "       -r 4

{
    !foo("$": {
        "_$_": [!bar()]
    })
}
```
bar.json
```json
{
    "H3IM3O66": {
        "_H3IM3O66_": [67, 86, 36, 61]
    },
    "QHZB5NMC": {
        "_QHZB5NMC_": [49, 24, 93, 77]
    },
    "JK2MSTAU": {
        "_JK2MSTAU_": [85, 86, 41, 56]
    },
    "GZA0V753": {
        "_GZA0V753_": [23, 19, 93, 24]
    },
    "79XOI38K": {
        "_79XOI38K_": [57, 61, 41, 3]
    },
    "0U2H5RLP": {
        "_0U2H5RLP_": [75, 72, 73, 63]
    },
    "8DLY976J": {
        "_8DLY976J_": [88, 38, 3, 64]
    },
    "5PU1MQW3": {
        "_5PU1MQW3_": [72, 27, 4, 30]
    },
    "IVG57ISE": {
        "_IVG57ISE_": [17, 37, 9, 43]
    }
}
```

---

```bash
rngstr --dsl foo.txt bar.txt 
```
foo.txt
```
!foo: --regex [0-2] -l 4 
!bar: --regex [7-9] -l 4
1: !foo()
2: !foo($)
3: !foo($ !foo())
4: !foo($ !bar())
5: !foo($ !bar($))
6: !foo(\$ !bar($))
7: \!foo($ !bar($))
8: \!foo($ \!bar($))
```
bar.txt
```
1: 2220
2: 0101
3: 0020 2201
4: 2022 9777
5: 0220 7879
6: $ 8897
7: !foo($ 7988)
8: !foo($ !bar($))
```

---

```bash
rngstr --dsl foo.txt bar.txt
```
foo.txt
```
!foo: -c 012 -l 4 
!r4:  -r 4 -s "\"" 

1: !foo() !foo() !foo() !foo()
2: !foo($ $ $ $)
3: !foo( abcd )
4: !r4(!foo())
5: !r4($ $ $ $)
6: !foo(!foo(!foo(!foo())))
7: !foo($ !foo($ !foo($ !foo())))
```
bar.txt
```
1: 1122 0212 1222 0202
2: 2211 2211 2211 2211
3: 0021 abcd 
4: 2021"1212"2201"1110
5:    "   "   "   
6: 0002
7: 0112 1220 2022 0201
```