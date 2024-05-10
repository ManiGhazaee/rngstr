use super::*;

pub fn cmd(args: &Vec<String>) -> String {
    let string = inside_quote(&args[0]);
    let args = cli_args(&string);
    let cli = Cli::parse_from(args);
    par_rngstr(&cli)
}

pub fn cmd_array(args: &Vec<String>) -> String {
    let string = inside_quote(&args[0]);
    let len: usize = args[1].parse().unwrap();
    let args = cli_args(&string);
    let cli = Cli::parse_from(args);
    let mut res = Vec::new();
    for _ in 0..len {
        res.push(par_rngstr(&cli));
    }
    format_array(&res)
}

pub fn f32(args: &Vec<String>) -> String {
    let (start, end) = parse_args!(args,
        0 => f32,
        1 => f32
    );
    thread_rng().gen_range(start..end).to_string()
}

pub fn i32(args: &Vec<String>) -> String {
    let (start, end) = parse_args!(args,
        0 => i32,
        1 => i32
    );
    thread_rng().gen_range(start..end).to_string()
}

pub fn u32(args: &Vec<String>) -> String {
    let (start, end) = parse_args!(args,
        0 => u32,
        1 => u32
    );
    thread_rng().gen_range(start..end).to_string()
}

pub fn f32_array(args: &Vec<String>) -> String {
    let (start, end, len) = parse_args!(args,
        0 => f32,
        1 => f32,
        2 => usize
    );
    format_array(&gen_range_to_string_vec!(start..end, len))
}

pub fn i32_array(args: &Vec<String>) -> String {
    let (start, end, len) = parse_args!(args,
        0 => i32,
        1 => i32,
        2 => usize
    );
    format_array(&gen_range_to_string_vec!(start..end, len))
}

pub fn u32_array(args: &Vec<String>) -> String {
    let (start, end, len) = parse_args!(args,
        0 => u32,
        1 => u32,
        2 => usize
    );
    format_array(&gen_range_to_string_vec!(start..end, len))
}
