#![allow(dead_code)]

use std::{f32::NAN, iter::Enumerate, ops::Range, str::Chars};

use clap::Parser;
use rand::{thread_rng, Rng};
use regex::Regex;

#[derive(Parser, Debug, Clone)]
#[command(
    version,
    about,
    long_about = "A cli tool that generates random strings of characters with options to customize the character set or length of the generated string.\nDefault character set = (A-Z, a-z, 0-9)\nDefault length = 32"
)]
#[command(next_line_help = true)]
pub struct Cli {
    #[arg(long, short, default_value_t = 32)]
    pub length: usize,

    /// Specify a string of custom characters (e.g. abc01111)
    #[arg(
        long,
        short,
        conflicts_with_all(&["password", "regex"])
    )]
    pub custom: Option<String>,

    /// Specify a regular expression pattern to be used to generate the character set (e.g. [0-9A-F] will generate 0123456789ABCDEF
    /// character set)
    #[arg(
        long,
        conflicts_with_all(&["password", "custom"]),
    )]
    pub regex: Option<String>,

    /// Specify a range between two unsigned integer with `start..end` syntax (`Range<usize>` start: inclusive, end: exclusive, start < end), (e.g. 24..36)
    #[arg(
        long,
        conflicts_with_all(&["password", "custom", "regex", "length"]),
    )]
    pub range: Option<String>,

    /// Specify a string to be prepended to the generated string
    #[arg(long, short, default_value_t = String::new())]
    pub prefix: String,

    /// Specify a string to be appended to the generated string
    #[arg(long, short, default_value_t = String::new())]
    pub suffix: String,

    /// Specify number of times string should be generated
    #[arg(long, short, default_value_t = 1)]
    pub repeat: usize,

    /// Don't copy the generated result to clipboard
    #[arg(long)]
    pub no_copy: bool,

    /// Don't print the generated result
    #[arg(long)]
    pub no_print: bool,

    #[arg(long)]
    pub no_trailing_suffix: bool,

    /// Use the password character set (A-Z, a-z, 0-9, and special characters)
    #[arg(long, conflicts_with_all(&["custom", "regex"]))]
    pub password: bool,

    #[arg(long, num_args(1..))]
    pub file: Option<Vec<String>>,
}

impl Cli {
    pub fn as_config(&self) -> Config {
        Config {
            no_trailing_suffix: self.no_trailing_suffix,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Config {
    pub no_trailing_suffix: bool,
}

#[derive(Debug, Default)]
pub struct Lexer {
    src: Vec<char>,
    cursor: usize,
    tokens: Vec<Token>,
    config: Config,
}

#[derive(Debug, Clone)]
pub enum Token {
    Command(String, Vec<Token>),
    String(String),
    Target,
}

#[derive(Debug, Clone, Default)]
pub struct Command {
    name: String,
    cli: Cli,
}

impl Default for Cli {
    fn default() -> Self {
        Self {
            length: 32,
            custom: None,
            regex: None,
            range: None,
            prefix: String::new(),
            suffix: String::new(),
            repeat: 1,
            password: false,
            no_copy: false,
            no_print: false,
            no_trailing_suffix: false,
            file: None,
        }
    }
}

pub fn tokenize(src: String, config: Config) -> (Vec<Command>, Vec<Token>) {
    let mut commands: Vec<Command> = Vec::new();
    for line in src.lines() {
        let line_string = line.to_string();
        let mut trimmed_line = line_string.trim_start();
        if trimmed_line.starts_with('!') {
            trimmed_line = &trimmed_line[1..];
            let mut temp = String::new();
            for (i, ch) in trimmed_line.chars().enumerate() {
                if ch == ':' {
                    let name = temp.trim().to_string();
                    let args = parse_args(&trimmed_line[i + 1..]);
                    let mut cli = Cli::parse_from(args);
                    overwrite_cli(&mut cli, config.clone());
                    let cmd = Command { name, cli };
                    commands.push(cmd);
                    break;
                }
                temp.push(ch);
            }
        } else if trimmed_line != "" {
            break;
        }
    }

    let mut i = 0;
    let bytes = src.as_bytes();
    let mut bang = false;
    while i < bytes.len() {
        match bytes[i] as char {
            '!' => bang = true,
            '\n' | '\r' => bang = false,
            ' ' | '\t' => (),
            _ => {
                if !bang {
                    break;
                }
            }
        }
        i += 1;
    }

    let src = &src[i..];

    let mut chars: Enumerate<Chars> = src.chars().enumerate();
    let mut tokens = Vec::new();
    let mut temp = String::new();
    while let Some((_, ch)) = chars.next() {
        match ch {
            '!' => {
                tokens.push(Token::String(temp.clone()));
                tokens.push(parse_command_call(&mut chars, &commands));
                temp.clear();
                continue;
            }
            _ => (),
        }
        temp.push(ch);
    }
    tokens.push(Token::String(temp.clone()));

    (commands, tokens)
}

fn parse_command_call(chars: &mut Enumerate<Chars>, commands: &[Command]) -> Token {
    let mut temp = String::new();
    while let Some((_, ch)) = chars.next() {
        if ch == '(' {
            break;
        }
        temp.push(ch);
    }
    let name = temp.trim().to_string();
    temp.clear();

    let mut paren = 1;
    let mut temp = temp;
    let mut value: Vec<Token> = Vec::new();
    while let Some((_, ch)) = chars.next() {
        match ch {
            '(' => paren += 1,
            ')' => paren -= 1,
            '$' => {
                value.push(Token::String(temp.clone()));
                value.push(Token::Target);
                temp.clear();
                continue;
            }
            '!' => {
                value.push(Token::String(temp.clone()));
                value.push(parse_command_call(chars, commands));
                temp.clear();
                continue;
            }
            _ => (),
        };
        if paren == 0 {
            if value.is_empty() || (value.len() == 1 && temp.trim().is_empty()) {
                value.clear();
                value.push(Token::Target);
            }
            break;
        }
        temp.push(ch);
    }
    value.push(Token::String(temp));

    Token::Command(name, value)
}

fn parse_args(string: &str) -> Vec<String> {
    let mut args = Vec::new();
    let mut chars = string.chars();
    let mut is_string = false;
    let mut temp = String::new();
    while let Some(ch) = chars.next() {
        match ch {
            '"' | '\'' => is_string = !is_string,
            ' ' | '\t' => {
                if !is_string && !temp.trim().is_empty() {
                    args.push(temp.trim().to_owned());
                    temp.clear();
                    continue;
                }
            }
            '\\' => {
                temp.push(ch);
                if let Some(ch) = chars.next() {
                    temp.push(ch);
                }
                continue;
            }
            _ => (),
        };
        temp.push(ch);
    }
    if !temp.trim().is_empty() {
        args.push(temp.trim().to_owned());
    }

    args.insert(0, "X".to_string());

    for arg in args.iter_mut() {
        let mut bytes = arg.as_bytes().to_vec();
        if bytes.is_empty() {
            continue;
        }
        let (first, last) = (bytes[0], bytes[bytes.len() - 1]);
        if !(first == last && (first == b'"' || first == b'\'')) {
            continue;
        }
        bytes = bytes[1..bytes.len() - 1].to_vec();
        let mut i = 0;
        while i < bytes.len() - 1 {
            if bytes[i] == b'\\' && (bytes[i + 1] == b'"' || bytes[i + 1] == b'\'') {
                bytes.remove(i);
            } else if bytes[i] == b'\\' && (bytes[i + 1] == b'n') {
                bytes.remove(i);
                bytes[i] = b'\n';
            } else if bytes[i] == b'\\' && (bytes[i + 1] == b'r') {
                bytes.remove(i);
                bytes[i] = b'\r';
            } else if bytes[i] == b'\\' && (bytes[i + 1] == b't') {
                bytes.remove(i);
                bytes[i] = b'\t';
            }
            i += 1;
        }
        *arg = String::from_utf8(bytes).unwrap();
    }
    // dbg!(&args);
    args
}

pub fn parse(commands: Vec<Command>, tokens: Vec<Token>, command: &Command) -> String {
    let mut res: String = String::new();
    for i in 0..command.cli.repeat {
        let mut temp = String::new();
        for token in &tokens {
            let x = match token {
                Token::Command(name, tokens) => parse(
                    commands.clone(),
                    tokens.clone(),
                    commands
                        .iter()
                        .find(|i| i.name.to_string() == name.to_string())
                        .expect("command not found"),
                ),
                Token::String(string) => string.to_string(),
                Token::Target => {
                    let str = rngstr(&Cli {
                        repeat: 1,
                        prefix: String::new(),
                        suffix: String::new(),
                        ..command.cli.clone()
                    });
                    temp.push_str(&str);
                    continue;
                }
            };
            temp.push_str(&x);
        }
        if (i == command.cli.repeat - 1) && command.cli.no_trailing_suffix {
            res.push_str(&format(&command.cli.prefix, &temp, ""));
        } else {
            res.push_str(&format(&command.cli.prefix, &temp, &command.cli.suffix));
        }
    }
    res
}

const SET: [char; 94] = [
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S',
    'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
    'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4',
    '5', '6', '7', '8', '9', '_', '-', '.', '~', '`', '!', '@', '#', '$', '%', '^', '&', '*', '(',
    ')', '+', '=', '{', '[', '}', ']', '|', '\\', ':', ';', '"', '\'', '<', ',', '>', '?', '/',
];

fn gen(len: usize, set: &[char]) -> String {
    let mut rng = thread_rng();
    let mut res = String::with_capacity(len);
    for _ in 0..len {
        res.push(set[rng.gen_range(0..set.len())]);
    }
    res
}

fn gen_custom(len: usize, set: &str) -> String {
    let mut rng = thread_rng();
    let mut res = String::with_capacity(len);
    let set_chars: Vec<char> = set.chars().collect();

    for _ in 0..len {
        res.push(*set_chars.get(rng.gen_range(0..set_chars.len())).unwrap());
    }

    res
}

fn format(prefix: &str, str: &str, suffix: &str) -> String {
    let mut res = String::with_capacity(prefix.len() + str.len() + suffix.len());
    res.push_str(prefix);
    res.push_str(str);
    res.push_str(suffix);
    res
}

fn parse_range(str: &str) -> Range<usize> {
    let vec = str
        .trim()
        .split("..")
        .map(|i| i.parse::<f32>().unwrap_or(NAN))
        .collect::<Vec<f32>>();
    let mut start = 0;
    let end;
    if !vec.get(0).expect("Error: parsing range").is_nan() {
        start = vec[0] as usize;
    };
    if vec.get(1).expect("Error: parsing range").is_nan() {
        end = usize::MAX;
    } else {
        end = vec[1] as usize;
    }

    Range { start, end }
}

pub fn rngstr(cli: &Cli) -> String {
    match (&cli.custom, &cli.regex, &cli.range, &cli.password) {
        (Some(set), ..) => (0..cli.repeat)
            .map(|_| format(&cli.prefix, &gen_custom(cli.length, &set), &cli.suffix))
            .collect::<String>(),
        (_, Some(re), ..) => {
            let re = Regex::new(&re).unwrap();
            let mut buf = [0; 4];
            let set: String = (0..=255)
                .map(|i| char::from(i))
                .filter(|ch| re.is_match(&ch.encode_utf8(&mut buf)))
                .collect();
            (0..cli.repeat)
                .map(|_| format(&cli.prefix, &gen_custom(cli.length, &set), &cli.suffix))
                .collect::<String>()
        }
        (.., Some(range), _) => {
            let range = parse_range(range);
            let mut rng = thread_rng();
            (0..cli.repeat)
                .map(|_| {
                    format(
                        &cli.prefix,
                        &rng.gen_range(range.clone()).to_string(),
                        &cli.suffix,
                    )
                })
                .collect::<String>()
        }
        (.., true) => (0..cli.repeat)
            .map(|_| format(&cli.prefix, &gen(cli.length, &SET), &cli.suffix))
            .collect::<String>(),
        _ => (0..cli.repeat)
            .map(|_| format(&cli.prefix, &gen(cli.length, &SET[0..62]), &cli.suffix))
            .collect::<String>(),
    }
}

fn overwrite_cli(cli: &mut Cli, config: Config) {
    cli.no_trailing_suffix = config.no_trailing_suffix;
}
