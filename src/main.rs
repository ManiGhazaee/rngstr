use clap::Parser;
use cli_clipboard::set_contents;
use rand::{thread_rng, Rng};

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
    long_about = "A command-line tool that generates random strings of characters with options to customize the character set or length of the generated string.\nDefault character set = (A-Z, a-z, 0-9)\nDefault length = 32"
)]
#[command(next_line_help = true)]
struct Cli {
    #[arg(long, short, default_value_t = 32)]
    length: usize,

    /// Use the password character set (A-Z, a-z, 0-9, and special characters)
    #[arg(long, short, conflicts_with_all(&["url", "custom"]))]
    password: bool,

    /// Use the URL character set (A-Z, a-z, 0-9, and -_.~)
    #[arg(long, short, conflicts_with_all(&["password", "custom"]))]
    url: bool,

    /// Specify a string of custom characters (e.g. abcd0123)
    #[arg(
        long,
        short,
        conflicts_with_all(&["password", "url"])
    )]
    custom: Option<String>,

    /// Don't copy the generated result to clipboard
    #[arg(long)]
    no_copy: bool,

    /// Don't print the generated result
    #[arg(long)]
    no_print: bool,
}

fn gen(len: usize, set: &[char]) -> String {
    let mut rng = thread_rng();
    let mut res = String::with_capacity(len);
    for _ in 0..len {
        res.push(set[rng.gen_range(0..set.len())]);
    }
    res
}

fn gen_custom(len: usize, set: String) -> String {
    let mut rng = thread_rng();
    let mut res = String::with_capacity(len);
    for _ in 0..len {
        res.push(
            set.chars()
                .nth(rng.gen_range(0..set.len()))
                .unwrap_or_default(),
        );
    }
    res
}

fn main() {
    let cli = Cli::parse();
    let len = cli.length;

    let res = if let Some(seed) = cli.custom {
        gen_custom(len, seed)
    } else if cli.password {
        gen(len, &SET)
    } else if cli.url {
        gen(len, &SET[0..66])
    } else {
        gen(len, &SET[0..62])
    };

    if !cli.no_print {
        println!("{}", res);
    }

    if !cli.no_copy {
        set_contents(res).unwrap();
    }
}
