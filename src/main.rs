use std::{fs, time::Instant};

use clap::Parser;
use rngstr::{commands, copy_print, par_rngstr, parse, tokenize, Cli};

fn main() {
    let cli = Cli::parse();
    if let Some(file) = &cli.dsl {
        let src = fs::read_to_string(&file[0]).expect("reading source file");
        let i = Instant::now();
        let commands = commands(&src, &cli.as_config());
        let tokens = tokenize(src);
        let res = parse(&tokens, &Default::default(), &commands);
        dbg!(i.elapsed());
        if file.len() == 1 {
            copy_print(&cli, res);
        } else if file.len() == 2 {
            fs::write(&file[1], res).expect("writing file in destination");
        } else {
            panic!("multiple files not supported");
        }
        return;
    }
    let res = par_rngstr(&cli);

    copy_print(&cli, res);
}
