use std::{fs, time::Instant};

use clap::Parser;
use rngstr::{procs, copy_print, par_rngstr, parse, tokenize, Config};

fn main() {
    let config = Config::parse();
    if let Some(file) = &config.dsl {
        let src = fs::read_to_string(&file[0]).expect("reading source file");
        let commands = procs(&src);
        let tokens = tokenize(src);
        let inst = Instant::now();
        let res = parse(&tokens, &Default::default(), &commands).unwrap();
        dbg!(inst.elapsed());
        if file.len() == 1 {
            copy_print(&config, res);
        } else if file.len() == 2 {
            fs::write(&file[1], res).expect("writing file in destination");
        } else {
            panic!("multiple files not supported");
        }
        return;
    }
    let res = par_rngstr(&config);

    copy_print(&config, res);
}
