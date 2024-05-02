use std::fs;

use clap::Parser;
use cli_clipboard::set_contents;
use rngstr::{parse, rngstr, tokenize, Cli};

fn main() {
    let cli = Cli::parse();
    if let Some(ref file) = cli.file {
        let src = fs::read_to_string(&file[0]).expect("Error: reading source file");
        let (commands, tokens) = tokenize(src, cli.as_config());
        let res = parse(commands, tokens, &Default::default());
        if file.len() == 1 {
            println!("{}", res);
        } else if file.len() == 2 {
            fs::write(&file[1], res).expect("Error: writing file in destination");
        } else {
            panic!("Error: multiple files not supported");
        }
        return;
    }
    let res = rngstr(&cli);

    if !cli.no_print {
        println!("{}", res);
    }

    if !cli.no_copy {
        set_contents(res).expect("Error: copying to clipboard");
    }
}
