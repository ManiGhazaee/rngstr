use clap::Parser;
use cli_clipboard::set_contents;
use rand::{thread_rng, Rng};
use regex::Regex;

const SET: [char; 94] = [
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S',
    'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
    'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4',
    '5', '6', '7', '8', '9', '_', '-', '.', '~', '`', '!', '@', '#', '$', '%', '^', '&', '*', '(',
    ')', '+', '=', '{', '[', '}', ']', '|', '\\', ':', ';', '"', '\'', '<', ',', '>', '?', '/',
];

#[derive(Parser)]
#[command(
    version,
    about,
    long_about = "A cli tool that generates random strings of characters with options to customize the character set or length of the generated string.\nDefault character set = (A-Z, a-z, 0-9)\nDefault length = 32"
)]
#[command(next_line_help = true)]
struct Cli {
    #[arg(long, short, default_value_t = 32)]
    length: usize,

    /// Specify a string of custom characters (e.g. abc01111)
    #[arg(
        long,
        short,
        conflicts_with_all(&["password", "regex"])
    )]
    custom: Option<String>,

    /// Specify a regular expression pattern to be used to generate the character set (e.g. [0-9A-F] will generate 0123456789ABCDEF
    /// character set)
    #[arg(
        long,
        conflicts_with_all(&["password", "custom"]),
    )]
    regex: Option<String>,

    /// Specify a string to be prepended to the generated string
    #[arg(long, short, default_value_t = String::new())]
    prefix: String,

    /// Specify a string to be appended to the generated string
    #[arg(long, short, default_value_t = String::new())]
    suffix: String,

    /// Specify number of times string should be generated
    #[arg(long, short, default_value_t = 1)]
    repeat: usize,

    /// Don't copy the generated result to clipboard
    #[arg(long)]
    no_copy: bool,

    /// Don't print the generated result
    #[arg(long)]
    no_print: bool,

    /// Use the password character set (A-Z, a-z, 0-9, and special characters)
    #[arg(long, conflicts_with_all(&["custom", "regex"]))]
    password: bool,
}

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

fn main() {
    let cli = Cli::parse();
    let len = cli.length;
    let repeat = cli.repeat;
    let prefix = cli.prefix;
    let suffix = cli.suffix;

    let res = match (cli.custom, cli.regex, cli.password) {
        (Some(set), ..) => (0..repeat)
            .map(|_| format(&prefix, &gen_custom(len, &set), &suffix))
            .collect::<String>(),
        (_, Some(re), _) => {
            let re = Regex::new(&re).unwrap();
            let mut buf = [0; 4];
            let set: String = (0..=255)
                .map(|i| char::from(i))
                .filter(|ch| re.is_match(&ch.encode_utf8(&mut buf)))
                .collect();
            (0..repeat)
                .map(|_| format(&prefix, &gen_custom(len, &set), &suffix))
                .collect::<String>()
        }
        (_, _, true) => (0..repeat)
            .map(|_| format(&prefix, &gen(len, &SET), &suffix))
            .collect::<String>(),
        _ => (0..repeat)
            .map(|_| format(&prefix, &gen(len, &SET[0..62]), &suffix))
            .collect::<String>(),
    };

    if !cli.no_print {
        println!("{}", res);
    }

    if !cli.no_copy {
        set_contents(res).unwrap();
    }
}
