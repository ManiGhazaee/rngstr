use core::panic;
use std::{
    collections::{HashMap, HashSet},
    f32::NAN,
    hash::Hash,
    iter::{Enumerate, Peekable},
    ops::Range,
    str::Chars,
    sync::{Arc, Mutex},
};

use clap::Parser;
use cli_clipboard::set_contents;
use rand::{rngs::ThreadRng, thread_rng, Rng};
use rayon::prelude::*;
use regex::bytes::Regex;

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref SET_CACHE: HashMap<String, String> = HashMap::new();
    static ref UTILS: Mutex<[(String, Command); 7]> = Mutex::new([
        (
            "repeat".into(),
            Command::Command {
                params: Some(Params {
                    raw: "--repeat count -s suffix".into(),
                    params: vec!["count".into(), "suffix".into()],
                }),
                cli: Default::default(),
            }
        ),
        (
            "u32".into(),
            Command::Builtin {
                params: Some(Params {
                    raw: "".into(),
                    params: vec!["start".into(), "end".into()],
                }),
                f: u32,
            }
        ),
        (
            "i32".into(),
            Command::Builtin {
                params: Some(Params {
                    raw: "".into(),
                    params: vec!["start".into(), "end".into()],
                }),
                f: i32,
            }
        ),
        (
            "f32".into(),
            Command::Builtin {
                params: Some(Params {
                    raw: "".into(),
                    params: vec!["start".into(), "end".into()],
                }),
                f: f32,
            }
        ),
        (
            "f32_array".into(),
            Command::Builtin {
                params: Some(Params {
                    raw: "".into(),
                    params: vec!["range_start".into(), "range_end".into(), "length".into()],
                }),
                f: f32_array,
            }
        ),
        (
            "i32_array".into(),
            Command::Builtin {
                params: Some(Params {
                    raw: "".into(),
                    params: vec!["range_start".into(), "range_end".into(), "length".into()],
                }),
                f: i32_array,
            }
        ),
        (
            "u32_array".into(),
            Command::Builtin {
                params: Some(Params {
                    raw: "".into(),
                    params: vec!["range_start".into(), "range_end".into(), "length".into()],
                }),
                f: u32_array,
            }
        )
    ]);
}

fn f32(args: &Vec<String>) -> String {
    let (start, end) = (
        args[0].parse::<f32>().unwrap(),
        args[1].parse::<f32>().unwrap(),
    );
    thread_rng().gen_range(start..end).to_string()
}

fn i32(args: &Vec<String>) -> String {
    let (start, end) = (
        args[0].parse::<i32>().unwrap(),
        args[1].parse::<i32>().unwrap(),
    );
    thread_rng().gen_range(start..end).to_string()
}

fn u32(args: &Vec<String>) -> String {
    let (start, end) = (
        args[0].parse::<u32>().unwrap(),
        args[1].parse::<u32>().unwrap(),
    );
    thread_rng().gen_range(start..end).to_string()
}

fn f32_array(args: &Vec<String>) -> String {
    let (start, end, len) = (
        args[0].parse::<f32>().unwrap(),
        args[1].parse::<f32>().unwrap(),
        args[2].parse::<usize>().unwrap(),
    );
    let mut res = Vec::new();
    let mut rng = thread_rng();
    for _ in 0..len {
        res.push(rng.gen_range(start..end).to_string());
    }
    let mut res = res.join(", ");
    res.insert_str(0, "[");
    res.push_str("]");
    res
}

fn i32_array(args: &Vec<String>) -> String {
    let (start, end, len) = (
        args[0].parse::<i32>().unwrap(),
        args[1].parse::<i32>().unwrap(),
        args[2].parse::<usize>().unwrap(),
    );
    let mut res = Vec::new();
    let mut rng = thread_rng();
    for _ in 0..len {
        res.push(rng.gen_range(start..end).to_string());
    }
    let mut res = res.join(", ");
    res.insert_str(0, "[");
    res.push_str("]");
    res
}

fn u32_array(args: &Vec<String>) -> String {
    let (start, end, len) = (
        args[0].parse::<u32>().unwrap(),
        args[1].parse::<u32>().unwrap(),
        args[2].parse::<usize>().unwrap(),
    );
    let mut res = Vec::new();
    let mut rng = thread_rng();
    for _ in 0..len {
        res.push(rng.gen_range(start..end).to_string());
    }
    let mut res = res.join(", ");
    res.insert_str(0, "[");
    res.push_str("]");
    res
}

type Commands = HashMap<String, Command>;

#[derive(Parser, Debug, Clone)]
#[command(
    version,
    about,
    long_about = "A cli tool for generating random strings of characters with customization options and a small domain specific language"
)]
#[command(next_line_help = true)]
pub struct Cli {
    #[arg(long, short, default_value_t = 0)]
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

    /// Set trailing suffix generation to true
    #[arg(long, short)]
    pub trailing_suffix: bool,

    /// Use the password character set (A-Z, a-z, 0-9, and special characters)
    #[arg(long, conflicts_with_all(&["custom", "regex"]))]
    pub password: bool,

    /// Specify path of the source file as first argument and optional second path argument for destination of the output
    #[arg(long, short, num_args(1..))]
    pub dsl: Option<Vec<String>>,
}

#[derive(Debug, Default, Clone)]
pub struct Config {
    pub trailing_suffix: bool,
}

#[derive(Debug, Clone)]
pub enum Token {
    Command {
        name: String,
        tokens: Vec<Token>,
        args: Option<Vec<Vec<Token>>>,
    },
    String(String),
    Target,
}

#[derive(Debug, Clone)]
pub enum Command {
    Command {
        params: Option<Params>,
        cli: Cli,
    },
    Macro {
        tokens: Vec<Token>,
    },
    Builtin {
        params: Option<Params>,
        f: fn(&Vec<String>) -> String,
    },
}

#[derive(Debug, Clone)]
pub struct Params {
    raw: String,
    params: Vec<String>,
}

impl Default for Command {
    fn default() -> Self {
        Command::Command {
            params: None,
            cli: Cli::default(),
        }
    }
}

impl Cli {
    pub fn as_config(&self) -> Config {
        Config {
            trailing_suffix: self.trailing_suffix,
        }
    }
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
            trailing_suffix: false,
            dsl: None,
        }
    }
}

fn split_trim_by(str: &str, pat: char) -> Vec<String> {
    str.split(pat)
        .filter_map(|i| {
            let t = i.trim();
            if !t.is_empty() {
                Some(t.to_string())
            } else {
                None
            }
        })
        .collect::<Vec<_>>()
}

pub fn tokenize(src: String, config: Config) -> (Commands, Vec<Token>) {
    let commands = commands(&src, &config);
    let src = &src[first_text_byte(&src)..];
    let tokens = tokenize_text(src, &commands);

    (commands, tokens)
}

fn first_text_byte(src: &str) -> usize {
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
    i
}

fn hashmap_from<K: Eq + Hash + Default, V: Default>(slice: &mut [(K, V)]) -> HashMap<K, V> {
    let mut h = HashMap::new();
    for i in 0..slice.len() {
        let s = std::mem::take(&mut slice[i]);
        h.insert(s.0, s.1);
    }
    h
}

fn commands(src: &str, config: &Config) -> Commands {
    let mut commands: Commands = hashmap_from(&mut *UTILS.lock().unwrap());
    for line in src.lines() {
        let line_string = line.to_string();
        let mut trimmed_line = line_string.trim_start();
        if trimmed_line.starts_with('!') {
            trimmed_line = &trimmed_line[1..];
            let mut name = String::new();
            let mut chars = trimmed_line.chars().enumerate().peekable();
            let mut params = None;
            while let Some((i, ch)) = chars.next() {
                match ch {
                    '[' => {
                        let mut temp = String::new();
                        while let Some((_, ch)) = chars.next() {
                            if ch == ']' {
                                break;
                            }
                            temp.push(ch);
                        }
                        params = Some(split_trim_by(&temp, ','));
                        continue;
                    }
                    ':' => {
                        let name = name.trim().to_string();
                        if let Some((_, '(')) = chars.peek() {
                            chars.next();
                            let mut paren = 1;
                            let mut temp = String::new();
                            while let Some((_, ch)) = chars.next() {
                                match ch {
                                    '(' => paren += 1,
                                    ')' => paren -= 1,
                                    _ => (),
                                }
                                if paren == 0 {
                                    break;
                                }
                                temp.push(ch);
                            }
                            let tokens = tokenize_text(&temp, &commands);
                            let command = Command::Macro { tokens };
                            commands.insert(name, command);
                        } else {
                            let raw_args = &trimmed_line[i + 1..];
                            if let Some(params) = params {
                                let command = Command::Command {
                                    params: Some(Params {
                                        raw: raw_args.to_string(),
                                        params,
                                    }),
                                    cli: Cli::default(),
                                };
                                commands.insert(name, command);
                            } else {
                                let args = parse_args(raw_args);
                                let mut cli = Cli::parse_from(args);
                                overwrite_cli(&mut cli, &config);
                                let command = Command::Command { params: None, cli };
                                commands.insert(name, command);
                            }
                        }
                        break;
                    }
                    _ => (),
                }
                name.push(ch);
            }
        } else if trimmed_line != "" {
            break;
        }
    }
    commands
}

fn tokenize_text(src: &str, commands: &Commands) -> Vec<Token> {
    let mut chars: Peekable<Enumerate<Chars>> = src.chars().enumerate().peekable();
    let mut tokens = Vec::new();
    let mut string_token = String::new();
    while let Some((_, ch)) = chars.next() {
        match ch {
            '!' => {
                tokens.push(Token::String(string_token.clone()));
                tokens.push(command_call_token(&mut chars, &commands));
                string_token.clear();
                continue;
            }
            '\\' => {
                if let Some((_, '!')) = chars.peek() {
                    string_token.push(chars.next().unwrap().1);
                    continue;
                }
            }
            _ => (),
        }
        string_token.push(ch);
    }
    tokens.push(Token::String(string_token.clone()));

    tokens
}

fn split_with_positions(str: &str, positions: HashSet<usize>) -> Vec<String> {
    let mut chars = str.chars().enumerate();
    let mut temp = 0;
    let mut res = Vec::new();
    while let Some((i, _)) = chars.next() {
        if positions.contains(&i) {
            res.push(str[temp..i].trim().to_string());
            temp = i + 1;
        }
    }
    res.push(str[temp..str.len()].trim().to_string());
    res
}

fn command_call_token(chars: &mut Peekable<Enumerate<Chars>>, commands: &Commands) -> Token {
    let mut name = String::new();
    let mut args = None::<Vec<Vec<Token>>>;
    while let Some((_, ch)) = chars.next() {
        match ch {
            '(' => break,
            '[' => {
                let mut brack = 1;
                let mut temp = String::new();
                let mut pos = 0;
                let mut comma_poses: HashSet<usize> = HashSet::new();
                let mut string = (false, '"');
                while let Some((_, ch)) = chars.next() {
                    match ch {
                        ',' => {
                            if brack == 1 && !string.0 {
                                comma_poses.insert(pos);
                            }
                        }
                        '"' | '\'' => {
                            if string.0 && string.1 == ch {
                                string = (false, '"');
                            } else {
                                string = (true, ch);
                            }
                        }
                        '[' => brack += 1,
                        ']' => brack -= 1,
                        _ => (),
                    }
                    if brack == 0 {
                        break;
                    }
                    temp.push(ch);
                    pos += 1;
                }
                let _args = split_with_positions(&temp, comma_poses);
                args = Some(
                    _args
                        .into_iter()
                        .map(|i| tokenize_text(&i, commands))
                        .collect::<Vec<_>>(),
                );
                continue;
            }
            _ => (),
        }
        name.push(ch);
    }
    let name = name.trim().to_string();

    let mut paren = 1;
    let mut string_token = String::new();
    let mut tokens: Vec<Token> = Vec::new();
    while let Some((_, ch)) = chars.next() {
        match ch {
            '(' => paren += 1,
            ')' => paren -= 1,
            '$' => {
                tokens.push(Token::String(string_token.clone()));
                tokens.push(Token::Target);
                string_token.clear();
                continue;
            }
            '!' => {
                tokens.push(Token::String(string_token.clone()));
                tokens.push(command_call_token(chars, commands));
                string_token.clear();
                continue;
            }
            '\\' => {
                if let Some((_, '$' | '!')) = chars.peek() {
                    string_token.push(chars.next().unwrap().1);
                    continue;
                }
            }
            _ => (),
        };
        if paren == 0 {
            if tokens.is_empty() || (tokens.len() == 1 && string_token.trim().is_empty()) {
                tokens.clear();
                tokens.push(Token::Target);
            }
            break;
        }
        string_token.push(ch);
    }
    tokens.push(Token::String(string_token));

    Token::Command { name, tokens, args }
}

fn parse_args(string: &str) -> Vec<String> {
    let mut args = Vec::new();
    let mut chars = string.chars();
    let mut is_string = false;
    let mut arg = String::new();
    while let Some(ch) = chars.next() {
        match ch {
            '"' | '\'' => is_string = !is_string,
            ' ' | '\t' => {
                if !is_string && !arg.trim().is_empty() {
                    args.push(arg.trim().to_owned());
                    arg.clear();
                    continue;
                }
            }
            '\\' => {
                arg.push(ch);
                if let Some(ch) = chars.next() {
                    arg.push(ch);
                }
                continue;
            }
            _ => (),
        };
        arg.push(ch);
    }
    if !arg.trim().is_empty() {
        args.push(arg.trim().to_owned());
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
        while i < bytes.len().saturating_sub(1) {
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
    args
}

pub fn parse(commands: &Commands, tokens: &Vec<Token>, command: &Command) -> String {
    match command {
        Command::Builtin { .. } => {
            panic!()
        }
        Command::Command { cli, .. } => (0..cli.repeat)
            .into_par_iter()
            .map(|i| {
                let gen: Arc<Mutex<Option<String>>> = Arc::new(Mutex::new(None));
                let temp = tokens
                    .into_par_iter()
                    .map(|token| match token {
                        Token::Command { name, tokens, args } => {
                            let command = commands
                                .get(name)
                                .expect(&format!("command '{}' not found", name));
                            let command = if let Some(args) = args {
                                match command {
                                    Command::Builtin { f, .. } => {
                                        let args = command_args(args, commands);
                                        return (f)(&args);
                                    }
                                    _ => {
                                        let cli =
                                            cli_from_command_args(args, command, name, commands);
                                        Command::Command { params: None, cli }
                                    }
                                }
                            } else {
                                command.to_owned()
                            };
                            parse(commands, tokens, &command)
                        }
                        Token::String(string) => string.to_string(),
                        Token::Target => {
                            let mut lock = gen.lock().unwrap();
                            if let Some(gen) = &*lock {
                                gen.to_string()
                            } else {
                                let str = rngstr(&Cli {
                                    repeat: 1,
                                    prefix: String::new(),
                                    suffix: String::new(),
                                    ..cli.clone()
                                });
                                *lock = Some(str.clone());
                                str
                            }
                        }
                    })
                    .collect::<String>();

                if (i == cli.repeat - 1) && !cli.trailing_suffix {
                    let ret = format(&cli.prefix, &temp, "");
                    ret
                } else {
                    let ret = format(&cli.prefix, &temp, &cli.suffix);
                    ret
                }
            })
            .collect::<String>(),
        Command::Macro { tokens } => parse(commands, tokens, &Command::default()),
    }
}

fn command_args(args: &Vec<Vec<Token>>, commands: &Commands) -> Vec<String> {
    args.iter()
        .map(|i| parse(commands, i, &Default::default()))
        .collect()
}

fn cli_from_command_args(
    args: &Vec<Vec<Token>>,
    command: &Command,
    name: &str,
    commands: &Commands,
) -> Cli {
    if let Command::Command {
        params: Some(params),
        ..
    } = command
    {
        let mut raw_args = params.raw.to_owned();
        for (i, p) in params.params.iter().enumerate() {
            raw_args = raw_args.replace(
                p,
                &parse(
                    commands,
                    args.get(i)
                        .expect(&format!("expected '{}' argument at command '{}'", p, name,)),
                    &Default::default(),
                ),
            );
        }
        let args = parse_args(&raw_args);
        let cli = Cli::parse_from(args);
        cli
    } else {
        panic!()
    }
}

const SET: [char; 94] = [
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S',
    'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
    'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4',
    '5', '6', '7', '8', '9', '_', '-', '.', '~', '`', '!', '@', '#', '$', '%', '^', '&', '*', '(',
    ')', '+', '=', '{', '[', '}', ']', '|', '\\', ':', ';', '"', '\'', '<', ',', '>', '?', '/',
];

fn gen(rng: &mut ThreadRng, len: usize, set: &[char]) -> String {
    let mut res = String::with_capacity(len);
    for _ in 0..len {
        res.push(set[rng.gen_range(0..set.len())]);
    }
    res
}

fn gen_custom(rng: &mut ThreadRng, len: usize, set: &str) -> String {
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
    if !vec.get(0).expect("parsing range").is_nan() {
        start = vec[0] as usize;
    };
    if vec.get(1).expect("parsing range").is_nan() {
        end = usize::MAX;
    } else {
        end = vec[1] as usize;
    }

    Range { start, end }
}

pub fn rngstr(cli: &Cli) -> String {
    let mut rng = thread_rng();
    match (&cli.custom, &cli.regex, &cli.range, &cli.password) {
        (Some(set), ..) => (0..cli.repeat)
            .map(|_| {
                format(
                    &cli.prefix,
                    &gen_custom(&mut rng, cli.length, &set),
                    &cli.suffix,
                )
            })
            .collect::<String>(),
        (_, Some(string), ..) => {
            let re = Regex::new(&string).unwrap();
            let set = if let Some(set) = SET_CACHE.get(string) {
                set.to_string()
            } else {
                let mut buf = [0; 4];
                (0..=255)
                    .map(|i| char::from(i))
                    .filter(|ch| re.is_match(ch.encode_utf8(&mut buf).as_bytes()))
                    .collect()
            };

            (0..cli.repeat)
                .map(|_| {
                    format(
                        &cli.prefix,
                        &gen_custom(&mut rng, cli.length, &set),
                        &cli.suffix,
                    )
                })
                .collect::<String>()
        }
        (.., Some(range), _) => {
            let range = parse_range(range);
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
            .map(|_| format(&cli.prefix, &gen(&mut rng, cli.length, &SET), &cli.suffix))
            .collect::<String>(),
        _ => (0..cli.repeat)
            .map(|_| {
                format(
                    &cli.prefix,
                    &gen(&mut rng, cli.length, &SET[0..62]),
                    &cli.suffix,
                )
            })
            .collect::<String>(),
    }
}

// clean code:
pub fn par_rngstr(cli: &Cli) -> String {
    if !cli.trailing_suffix {
        let take = cli.repeat.saturating_sub(1);
        match (&cli.custom, &cli.regex, &cli.range, &cli.password) {
            (Some(set), ..) => {
                let mut res = (0..cli.repeat)
                    .into_par_iter()
                    .take(take)
                    .map_init(
                        || thread_rng(),
                        |rng, _| {
                            format(&cli.prefix, &gen_custom(rng, cli.length, &set), &cli.suffix)
                        },
                    )
                    .collect::<String>();

                res.push_str(&format(
                    &cli.prefix,
                    &gen_custom(&mut thread_rng(), cli.length, &set),
                    "",
                ));
                res
            }
            (_, Some(string), ..) => {
                let re = Regex::new(&string).unwrap();
                let set: String = if let Some(set) = SET_CACHE.get(string) {
                    set.to_string()
                } else {
                    (0..=255)
                        .into_par_iter()
                        .map(|i| char::from(i))
                        .filter(|ch| re.is_match(ch.encode_utf8(&mut [0; 4]).as_bytes()))
                        .collect()
                };

                let mut res = (0..cli.repeat)
                    .into_par_iter()
                    .take(take)
                    .map_init(
                        || thread_rng(),
                        |rng, _| {
                            format(&cli.prefix, &gen_custom(rng, cli.length, &set), &cli.suffix)
                        },
                    )
                    .collect::<String>();

                res.push_str(&format(
                    &cli.prefix,
                    &gen_custom(&mut thread_rng(), cli.length, &set),
                    "",
                ));
                res
            }
            (.., Some(range), _) => {
                let range = parse_range(range);
                let mut res = (0..cli.repeat)
                    .into_par_iter()
                    .take(take)
                    .map_init(
                        || thread_rng(),
                        |rng, _| {
                            format(
                                &cli.prefix,
                                &rng.gen_range(range.clone()).to_string(),
                                &cli.suffix,
                            )
                        },
                    )
                    .collect::<String>();

                res.push_str(&format(
                    &cli.prefix,
                    &thread_rng().gen_range(range.clone()).to_string(),
                    "",
                ));
                res
            }
            (.., true) => {
                let mut res = (0..cli.repeat)
                    .into_par_iter()
                    .take(take)
                    .map_init(
                        || thread_rng(),
                        |rng, _| format(&cli.prefix, &gen(rng, cli.length, &SET), &cli.suffix),
                    )
                    .collect::<String>();

                res.push_str(&format(
                    &cli.prefix,
                    &gen(&mut thread_rng(), cli.length, &SET),
                    "",
                ));
                res
            }
            _ => {
                let mut res = (0..cli.repeat)
                    .into_par_iter()
                    .take(take)
                    .map_init(
                        || thread_rng(),
                        |rng, _| {
                            format(&cli.prefix, &gen(rng, cli.length, &SET[0..62]), &cli.suffix)
                        },
                    )
                    .collect::<String>();

                res.push_str(&format(
                    &cli.prefix,
                    &gen(&mut thread_rng(), cli.length, &SET[0..62]),
                    "",
                ));
                res
            }
        }
    } else {
        match (&cli.custom, &cli.regex, &cli.range, &cli.password) {
            (Some(set), ..) => (0..cli.repeat)
                .into_par_iter()
                .map_init(
                    || thread_rng(),
                    |rng, _| format(&cli.prefix, &gen_custom(rng, cli.length, &set), &cli.suffix),
                )
                .collect::<String>(),
            (_, Some(string), ..) => {
                let re = Regex::new(&string).unwrap();
                let set = if let Some(set) = SET_CACHE.get(string) {
                    set.to_string()
                } else {
                    (0..=255)
                        .into_par_iter()
                        .map(|i| char::from(i))
                        .filter(|ch| re.is_match(ch.encode_utf8(&mut [0; 4]).as_bytes()))
                        .collect()
                };

                (0..cli.repeat)
                    .into_par_iter()
                    .map_init(
                        || thread_rng(),
                        |rng, _| {
                            format(&cli.prefix, &gen_custom(rng, cli.length, &set), &cli.suffix)
                        },
                    )
                    .collect::<String>()
            }
            (.., Some(range), _) => {
                let range = parse_range(range);
                (0..cli.repeat)
                    .into_par_iter()
                    .map_init(
                        || thread_rng(),
                        |rng, _| {
                            format(
                                &cli.prefix,
                                &rng.gen_range(range.clone()).to_string(),
                                &cli.suffix,
                            )
                        },
                    )
                    .collect::<String>()
            }
            (.., true) => (0..cli.repeat)
                .into_par_iter()
                .map_init(
                    || thread_rng(),
                    |rng, _| format(&cli.prefix, &gen(rng, cli.length, &SET), &cli.suffix),
                )
                .collect::<String>(),
            _ => (0..cli.repeat)
                .into_par_iter()
                .map_init(
                    || thread_rng(),
                    |rng, _| format(&cli.prefix, &gen(rng, cli.length, &SET[0..62]), &cli.suffix),
                )
                .collect::<String>(),
        }
    }
}

fn overwrite_cli(cli: &mut Cli, config: &Config) {
    if config.trailing_suffix {
        cli.trailing_suffix = true;
    }
}

pub fn copy_print(cli: &Cli, res: String) {
    if !cli.no_print {
        println!("{}", res);
    }
    if !cli.no_copy {
        set_contents(res).expect("copying to clipboard");
    }
}
