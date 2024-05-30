# rngstr

A cli tool for generating random strings of characters with customization options and a small domain specific language.

## Table of Contents
- [Installation](#installation)
- [Usage](#usage)
  - [Options](#options)
  - [Defaults](#defaults)
- [Examples](#examples)
- [DSL Usage](#dsl-usage)
  - [Procedure Declaration](#procedure-declaration)
    - [Command](#command)
    - [Macro](#macro)
  - [Procedure Call](#procedure-call)
    - [Built-ins](#built-ins)
- [DSL Examples](#dsl-examples)

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

- `-g, --group <GROUP>...`:
          Specify a group of strings (e.g. "foo" "bar" "baz" will generate "bar")

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

### Procedure Declaration

#### Command

Basic:
```
!<name>: [OPTIONS]
```

Example:

```
!foo: --regex [0-9] -l 8
```

With parameters:

```
!<name><parameters...>: [OPTIONS]
```

Example:

```
!foo<start, end>: --range start..end
```

#### Macro

Basic:

```
!<name>:(<text>) 
```

Example:

```
!foo:(!bar())
```

With parameters:

```
!<name><parameters...>:(<text>)
```

Example:

```
!foo<x, y, z>:(!bar<z>() !baz<x, y>()) 
```

### Procedure Call

After the first non-declaration line (a trimmed line not starting with `!`), you can call the declared procedure using the following syntax:

Basic:

```
!<name>()
```

Example:

```
!foo()
```

With placeholder `$`:

```
!<name>($)
```

Example:

```
!foo($)
```

You can also use multiple placeholders within a call, where each `$` will generate the same string:

Example:

```
!foo: -c 012 -l 4
-
!foo($ $ $ $)
```

Output:
```
-
2211 2211 2211 2211
```

With arguments seperated by `,`:

```
!<name><arguments...>()
```

Example:

```
!foo<x, y, z>()
```

---

If we call the substring after the first `:` in the procedure declaration procedure body, each occurrence of parameters inside the procedure body is replaced by parsed arguments in the procedure call. It is possible to pass the name of a procedure as an argument and call it inside the procedure body:

Example:

```
!array<procedure, length>:([!repeat<length, ", ">(!procedure())])
!foo: --regex [a-z0-9] -l 8
!foo_str:("!foo()")

{
    "array": !array<foo_str, 4>()
}
```

Output:

```
{
    "array": ["2r4xtqv0", "t9na5pn0", "p1nbqvra", "c6hhww19"]
}
```

Or:

```
{
    "array": !array<array<array<foo_str, 2>, 2>, 2>()
}
```

Output:

```
{
    "array": [[["mw4kghh2", "qk2htxp2"], ["4s7g0z9n", "a1cszc89"]], [["xlcyhv4x", "ds4b351r"], ["pylnsvuu", "kny0h3a3"]]]
}
```

#### Built-ins

These procedures are usable by calling with their name and overwritable by declaraing another procedure with their name.

**`cmd<string>`**

Executes a command and returns the result.

Example:

```
!cmd<"--group foo bar baz">()
```

Output:

```
foo
```

---

**`repeat<count, suffix>`**

Repeats a procedure a specified number of times, with each result separated by a given suffix.

Example:

```
!repeat<4, ", ">(!cmd<"--range 0..100">())
```

Output: 

```
23, 67, 4, 91
```

---

**`array<procedure, length>`**

Creates an array by repeating a procedure a specified number of times.

Example:

```
!array<usize<0, 100>, 5>()
```

Output:

```
[23, 67, 4, 91, 45]
```

---

**`array_fill<element, length>`**

Creates an array filled with a specified element repeated a specified number of times.

Example:

```
!array_fill<42, 5>()
```

Output:

```
[42, 42, 42, 42, 42]
```


---

**`!id<tag>`**

Adds an entry in a hashmap with the tag as the key and 0 as the initial value, incrementing it on each call. Due to parallel parsing, the numbers are not generated in order.

Example:

```
!array<id<a>, 10>()
!array<id<b>, 10>()
!array<id<a>, 10>()
```

Output:

```
[10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```

---

**`!group<items...>`**

Randomly returns one of the specified items

Example:

```
!group<foo, bar, baz>()
!group<foo, bar, baz>()
```

Output:

```
foo
baz
```

---

**Number Types**

All Rust number types are supported for generating random values within a range or without a specified range:

Example:

```
[!u8<0, 10>(), !u16<0, 10>(), !u32<0, 10>(), !u64<0, 10>(), !u128<0, 10>(), !i8<0, 10>(), !i16<0, 10>(), !i32<0, 10>(), !i64<0, 10>(), !i128<0, 10>(), !f32<0, 10>(), !f64<0, 10>(), !usize<0, 10>(), !isize<0, 10>(), !id<0>()]
[!u8(), !u16(), !u32(), !u64(), !u128(), !i8(), !i16(), !i32(), !i64(), !i128(), !f32(), !f64(), !usize(), !isize()]
```

Ouput:

```
[3, 7, 4, 8, 9, 4, 7, 3, 1, 4, 6.409465, 1.9037502953455854, 3, 5, 0]
[169, 8026, 3981710656, 15859526283082379939, 294250989955636718152642071911969640664, 114, -11975, 862154430, -650703279019604957, 104795060125221781861534722358106096176, 0.46059388, 0.8589771759897691, 13709073818113593800, 5359762487833681572]
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

foo.txt
```
!foo_group:(foo, bar, baz)
!dq<proc>:("!proc()")

{
    "group": !array<dq<group<!foo_group()>>, 5>()
}
```
bar.json
```
{
    "group": ["foo", "baz", "bar", "foo", "foo"]
}
```

---

```bash
rngstr --dsl foo.txt bar.ron
```
foo.txt
```
!str:("!cmd<--regex [a-z] -l !usize<3, 9>()>()")
!rgba:(Rgba(!repeat<4, ", ">(!u8())))
!ida:("a!id<a>()")
!bool:(!group<true, false>())

( 
    foo: {
        !repeat<5, ",\n\t\t">(!str(): (
            x: !f32(),
        ))
    },
    bar: [
        !repeat<5, ",\n\t\t">((
            t: !rgba(),
            u: !ida(),
            v: !bool(),
        ))
    ],
)
```
bar.ron
```
( 
    foo: {
        "dxpln": (
            x: 0.7885895,
        ),
        "yaqdg": (
            x: 0.48093498,
        ),
        "hungukuf": (
            x: 0.9161161,
        ),
        "qtfnkeve": (
            x: 0.9836798,
        ),
        "dqedis": (
            x: 0.42920595,
        )
    },
    bar: [
        (
            t: Rgba(217, 101, 149, 83),
            u: "a0",
            v: true,
        ),
        (
            t: Rgba(209, 48, 76, 67),
            u: "a1",
            v: true,
        ),
        (
            t: Rgba(62, 167, 240, 81),
            u: "a2",
            v: false,
        ),
        (
            t: Rgba(251, 139, 57, 219),
            u: "a3",
            v: false,
        ),
        (
            t: Rgba(146, 106, 246, 52),
            u: "a4",
            v: false,
        )
    ],
)
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

