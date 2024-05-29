use std::{
    collections::{HashMap, HashSet},
    iter::{Enumerate, Peekable},
    ops::Range,
    str::Chars,
    sync::{Arc, Mutex, RwLock},
};

use builtins::BUILTINS;
use clap::Parser;
use cli_clipboard::set_contents;
use lazy_static::lazy_static;
use rand::{rngs::ThreadRng, thread_rng, Rng};
use rayon::prelude::*;
use regex::bytes::Regex;

#[macro_use]
mod macros;
mod builtins;

lazy_static! {
    static ref SET_CACHE: Mutex<HashMap<String, &'static str>> = Default::default();
    static ref TOKENIZER_CACHE: RwLock<HashMap<String, Vec<Token>>> = Default::default();
    static ref CONFIG_PARSER_CACHE: Mutex<HashMap<String, Config>> = Default::default();
    static ref IDS: Mutex<HashMap<String, usize>> = Default::default();
}

#[derive(Parser, Debug, Clone)]
#[command(
    version,
    about,
    long_about = "A cli tool for generating random strings of characters with customization options and a small domain specific language"
)]
#[command(next_line_help = true)]
pub struct Config {
    #[arg(long, short, default_value_t = 0)]
    pub length: usize,

    /// Specify a string of custom characters (e.g. abc01111)
    #[arg(
        long,
        short,
        conflicts_with_all(&["password", "regex", "group"])
    )]
    pub custom: Option<String>,

    /// Specify a regular expression pattern to be used to generate the character set (e.g. [0-9A-F] will generate 0123456789ABCDEF
    /// character set)
    #[arg(
        long,
        conflicts_with_all(&["password", "custom", "group"]),
    )]
    pub regex: Option<String>,

    /// Specify a group of strings (e.g. "foo" "bar" "baz" will generate "bar")
    #[arg(
        long,
        short,
        num_args(1..),
        conflicts_with_all(&["password", "custom", "regex", "length"])
    )]
    pub group: Option<Vec<String>>,

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

#[derive(Debug, Clone)]
pub enum Token {
    Call {
        name: String,
        tokens: Vec<Token>,
        args: Option<Vec<Vec<Token>>>,
    },
    String(&'static str),
    Target,
}

#[derive(Debug, Clone)]
pub enum Proc {
    Command {
        params: Option<Params>,
        config: Config,
    },
    Macro {
        params: Option<Params>,
        tokens: Vec<Token>,
    },
    Builtin {
        params: Option<Params>,
        f: fn(&Vec<String>) -> Result<String, String>,
    },
}

#[derive(Debug, Clone)]
pub struct Params {
    body: String,
    params: Vec<String>,
}

type Procs = HashMap<String, Proc>;

impl Default for Proc {
    fn default() -> Self {
        Proc::Command {
            params: None,
            config: Config::default(),
        }
    }
}

impl Config {
    pub fn from_body(raw: &str) -> Self {
        let mut lock = CONFIG_PARSER_CACHE.lock().unwrap();
        if let Some(config) = lock.get(raw) {
            return config.clone();
        } else {
            let args = config_args(raw);
            let config = Config::parse_from(args);
            lock.insert(raw.to_string(), config.clone());
            return config;
        }
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            length: 0,
            custom: None,
            regex: None,
            group: None,
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

pub fn tokenize(src: String) -> Vec<Token> {
    let src = &src[first_text_byte(&src)..];
    let tokens = tokenize_text(src);

    tokens
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

pub fn procs(src: &str) -> Procs {
    let mut procs = HashMap::from(std::mem::take(&mut *BUILTINS.lock().unwrap()));
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
                    '<' => {
                        let mut temp = String::new();
                        while let Some((_, ch)) = chars.next() {
                            if ch == '>' {
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
                            let proc = if let Some(p) = params {
                                Proc::Macro {
                                    tokens: Vec::new(),
                                    params: Some(Params {
                                        body: temp.to_string(),
                                        params: p,
                                    }),
                                }
                            } else {
                                let tokens = tokenize_text(&temp);
                                Proc::Macro {
                                    params: None,
                                    tokens,
                                }
                            };
                            procs.insert(name, proc);
                        } else {
                            let body = &trimmed_line[i + 1..];
                            if let Some(params) = params {
                                let proc = Proc::Command {
                                    params: Some(Params {
                                        body: body.to_string(),
                                        params,
                                    }),
                                    config: Config::default(),
                                };
                                procs.insert(name, proc);
                            } else {
                                let config = Config::from_body(body);
                                let proc = Proc::Command {
                                    params: None,
                                    config,
                                };
                                procs.insert(name, proc);
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

    procs
}

fn tokenize_text(src: &str) -> Vec<Token> {
    if let Some(tokens) = TOKENIZER_CACHE.read().unwrap().get(src) {
        return tokens.clone();
    }
    let mut chars: Peekable<Enumerate<Chars>> = src.chars().enumerate().peekable();
    let mut tokens = Vec::new();
    let mut string_token = String::new();
    while let Some((_, ch)) = chars.next() {
        match ch {
            '!' => {
                tokens.push(Token::String(leak(std::mem::take(&mut string_token))));
                tokens.push(call_token(&mut chars));
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
    if !string_token.is_empty() {
        tokens.push(Token::String(leak(string_token)));
    }
    TOKENIZER_CACHE
        .write()
        .unwrap()
        .insert(src.to_string(), tokens.clone());

    tokens
}

fn call_token(chars: &mut Peekable<Enumerate<Chars>>) -> Token {
    let mut name = String::new();
    let mut args = None::<Vec<Vec<Token>>>;
    while let Some((_, ch)) = chars.next() {
        match ch {
            '(' => break,
            '<' => {
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
                        '<' => brack += 1,
                        '>' => brack -= 1,
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
                        .map(|i| tokenize_text(&i))
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
                tokens.push(Token::String(leak(std::mem::take(&mut string_token))));
                tokens.push(Token::Target);
                continue;
            }
            '!' => {
                tokens.push(Token::String(leak(std::mem::take(&mut string_token))));
                tokens.push(call_token(chars));
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
    tokens.push(Token::String(leak(string_token)));

    Token::Call { name, tokens, args }
}

fn config_args(string: &str) -> Vec<String> {
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

pub fn parse(tokens: &Vec<Token>, proc: &Proc, procs: &Procs) -> Result<String, String> {
    match proc {
        Proc::Command { config, .. } => (0..config.repeat)
            .into_par_iter()
            .map(|i| {
                let generated: Arc<Mutex<Option<String>>> = Arc::new(Mutex::new(None));
                let parsed_tokens = tokens
                    .into_par_iter()
                    .map(|token| parse_token(token, config, procs, generated.clone()))
                    .collect::<Result<String, _>>()?;

                if (i == config.repeat - 1) && !config.trailing_suffix {
                    let ret = format(&config.prefix, &parsed_tokens, "");
                    Ok(ret)
                } else {
                    let ret = format(&config.prefix, &parsed_tokens, &config.suffix);
                    Ok(ret)
                }
            })
            .collect::<Result<String, _>>(),
        Proc::Macro { tokens, .. } => parse(tokens, &Proc::default(), procs),
        _ => unreachable!(),
    }
}

fn parse_token(
    token: &Token,
    config: &Config,
    procs: &Procs,
    generated: Arc<Mutex<Option<String>>>,
) -> Result<String, String> {
    match token {
        Token::Call { name, tokens, args } => {
            let proc = if let Some(proc) = procs.get(name) {
                proc
            } else {
                return Err(format!("command '{}' not found", name));
            };
            let proc = if let Some(args) = args {
                match proc {
                    Proc::Builtin { f, .. } => {
                        let args = parse_proc_args(args, procs)?;
                        return (f)(&args);
                    }
                    Proc::Macro {
                        params: Some(params),
                        ..
                    } => {
                        let tokens = replace_body_then(name, args, params, procs, |new_body| {
                            tokenize_text(&new_body)
                        })?;
                        Proc::Macro {
                            params: None,
                            tokens,
                        }
                    }
                    Proc::Command {
                        params: Some(params),
                        ..
                    } => {
                        let config = replace_body_then(name, args, params, procs, |new_body| {
                            Config::from_body(&new_body)
                        })?;
                        Proc::Command {
                            params: None,
                            config,
                        }
                    }
                    _ => unreachable!(),
                }
            } else {
                match proc {
                    Proc::Builtin { f, .. } => {
                        return (f)(&Default::default());
                    }
                    _ => proc.to_owned(),
                }
            };
            parse(tokens, &proc, procs)
        }
        Token::String(string) => Ok(string.to_string()),
        Token::Target => {
            let mut lock = generated.lock().unwrap();
            if let Some(gen) = &*lock {
                Ok(gen.to_string())
            } else {
                let str = rngstr_once(&config);
                *lock = Some(str.clone());
                Ok(str)
            }
        }
    }
}

fn parse_proc_args(args: &Vec<Vec<Token>>, procs: &Procs) -> Result<Vec<String>, String> {
    args.iter()
        .map(|i| parse(i, &Default::default(), procs))
        .collect::<Result<_, _>>()
}

fn replace_body_then<F: Fn(String) -> R, R>(
    name: &str,
    args: &Vec<Vec<Token>>,
    params: &Params,
    procs: &Procs,
    cb: F,
) -> Result<R, String> {
    let mut body = params.body.to_owned();
    for (i, p) in params.params.iter().enumerate() {
        body = body.replace(
            p,
            &parse(
                args.get(i)
                    .expect(&format!("expected '{}' argument at command '{}'", p, name,)),
                &Default::default(),
                procs,
            )?,
        );
    }
    Ok(cb(body))
}

const SET: [char; 94] = [
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S',
    'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
    'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4',
    '5', '6', '7', '8', '9', '_', '-', '.', '~', '`', '!', '@', '#', '$', '%', '^', '&', '*', '(',
    ')', '+', '=', '{', '[', '}', ']', '|', '\\', ':', ';', '"', '\'', '<', ',', '>', '?', '/',
];

fn parse_range(str: &str) -> Range<usize> {
    let mut iter = str.trim().split("..").map(|s| s.parse::<usize>());
    let start = match iter.next() {
        Some(Ok(start)) => start,
        _ => 0,
    };
    let end = match iter.next() {
        Some(Ok(end)) => end,
        _ => usize::MAX,
    };
    Range { start, end }
}

fn par_rngstr_from_str_generator(config: &Config, gen: &StrGenerator) -> String {
    (0..config.repeat)
        .into_par_iter()
        .take(config.repeat.saturating_sub(1))
        .map_init(
            || thread_rng(),
            |rng, _| format(&config.prefix, &gen.generate(rng), &config.suffix),
        )
        .collect::<String>()
}

fn set_from_regex<'a>(string: &'a str) -> &'static str {
    let mut lock = SET_CACHE.lock().unwrap();
    if let Some(set) = lock.get(string) {
        set
    } else {
        let re = Regex::new(&string).unwrap();
        let str = (0..=255)
            .into_par_iter()
            .map(char::from)
            .filter(|ch| re.is_match(ch.encode_utf8(&mut [0; 4]).as_bytes()))
            .collect::<String>();
        let static_str = leak(str);
        lock.insert(string.to_string(), static_str);
        static_str
    }
}

enum StrGenerator {
    Custom { set: String, length: usize },
    Regex { set: &'static str, length: usize },
    Set { set: &'static [char], length: usize },
    Group { group: Vec<String> },
    Range { range: Range<usize> },
}

impl StrGenerator {
    fn generate(&self, rng: &mut ThreadRng) -> String {
        match self {
            StrGenerator::Custom { set, length } => Self::from_custom(rng, *length, set),
            StrGenerator::Regex { set, length } => Self::from_custom(rng, *length, set),
            StrGenerator::Set { set, length } => Self::from_set(rng, *length, set),
            StrGenerator::Group { group } => Self::from_group(rng, group),
            StrGenerator::Range { range } => Self::from_range(rng, range),
        }
    }

    #[inline]
    fn from_set(rng: &mut ThreadRng, len: usize, set: &[char]) -> String {
        let mut res = String::with_capacity(len);
        for _ in 0..len {
            res.push(set[rng.gen_range(0..set.len())]);
        }
        res
    }

    #[inline]
    fn from_custom(rng: &mut ThreadRng, len: usize, set: &str) -> String {
        let mut res = String::with_capacity(len);
        let set_chars: Vec<char> = set.chars().collect();
        for _ in 0..len {
            res.push(*set_chars.get(rng.gen_range(0..set_chars.len())).unwrap());
        }
        res
    }

    #[inline]
    fn from_group(rng: &mut ThreadRng, group: &[String]) -> String {
        group[rng.gen_range(0..group.len())].to_string()
    }

    #[inline]
    fn from_range(rng: &mut ThreadRng, range: &Range<usize>) -> String {
        rng.gen_range(range.clone()).to_string()
    }
}

fn string_generator_from_config(config: &Config) -> StrGenerator {
    if let Some(set) = &config.custom {
        StrGenerator::Custom {
            set: set.to_string(),
            length: config.length,
        }
    } else if let Some(string) = &config.regex {
        let set = set_from_regex(string);
        StrGenerator::Regex {
            set,
            length: config.length,
        }
    } else if let Some(group) = &config.group {
        StrGenerator::Group {
            group: group.clone(),
        }
    } else if let Some(range) = &config.range {
        let range = parse_range(range);
        StrGenerator::Range { range }
    } else if config.password {
        StrGenerator::Set {
            set: &SET,
            length: config.length,
        }
    } else {
        StrGenerator::Set {
            set: &SET[0..62],
            length: config.length,
        }
    }
}

fn rngstr_once(config: &Config) -> String {
    let gen = string_generator_from_config(&config);
    let mut rng = thread_rng();
    gen.generate(&mut rng)
}

pub fn par_rngstr(config: &Config) -> String {
    let gen = string_generator_from_config(&config);
    let mut rng = thread_rng();
    let last = format(
        &config.prefix,
        &gen.generate(&mut rng),
        if config.trailing_suffix {
            &config.suffix
        } else {
            ""
        },
    );
    let mut res = par_rngstr_from_str_generator(config, &gen);
    res.push_str(&last);
    res
}

fn format(prefix: &str, str: &str, suffix: &str) -> String {
    let mut res = String::with_capacity(prefix.len() + str.len() + suffix.len());
    res.push_str(prefix);
    res.push_str(str);
    res.push_str(suffix);
    res
}

pub fn copy_print(config: &Config, res: String) {
    if !config.no_print {
        println!("{}", res);
    }
    if !config.no_copy {
        set_contents(res).expect("copying to configpboard");
    }
}

fn inside_quote(str: &str) -> String {
    let mut string = str.trim().to_string();
    let mut chars: Vec<_> = string.chars().collect();
    if let Some('"' | '\'') = chars.get(0) {
        chars.remove(0);
        chars.pop();
        string = chars.into_iter().collect::<String>();
    };
    string
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

fn leak<T>(val: T) -> &'static mut T {
    let b = Box::new(val);
    Box::leak(b)
}
