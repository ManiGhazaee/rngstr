use super::*;

lazy_static! {
    pub static ref BUILTINS: Mutex<[(String, Command); 11]> = Mutex::new([
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
            "cmd".into(),
            Command::Builtin {
                params: Some(Params {
                    raw: "".into(),
                    params: vec!["string".into()],
                }),
                f: builtins::cmd,
            }
        ),
        (
            "cmd_array".into(),
            Command::Builtin {
                params: Some(Params {
                    raw: "".into(),
                    params: vec!["string".into(), "length".into()],
                }),
                f: builtins::cmd_array,
            }
        ),
        (
            "array".into(),
            Command::Macro {
                params: Some(Params {
                    raw: "[!repeat<length, \", \">(!command())]".into(),
                    params: vec!["command".into(), "length".into(),],
                },),
                tokens: vec![],
            }
        ),
        (
            "array_fill".into(),
            Command::Macro {
                params: Some(Params {
                    raw: "[!repeat<length, \", \">(element)]".into(),
                    params: vec!["element".into(), "length".into(),],
                },),
                tokens: vec![],
            }
        ),
        (
            "u32".into(),
            Command::Builtin {
                params: Some(Params {
                    raw: "".into(),
                    params: vec!["start".into(), "end".into()],
                }),
                f: builtins::u32,
            }
        ),
        (
            "i32".into(),
            Command::Builtin {
                params: Some(Params {
                    raw: "".into(),
                    params: vec!["start".into(), "end".into()],
                }),
                f: builtins::i32,
            }
        ),
        (
            "f32".into(),
            Command::Builtin {
                params: Some(Params {
                    raw: "".into(),
                    params: vec!["start".into(), "end".into()],
                }),
                f: builtins::f32,
            }
        ),
        (
            "f32_array".into(),
            Command::Builtin {
                params: Some(Params {
                    raw: "".into(),
                    params: vec!["range_start".into(), "range_end".into(), "length".into()],
                }),
                f: builtins::f32_array,
            }
        ),
        (
            "i32_array".into(),
            Command::Builtin {
                params: Some(Params {
                    raw: "".into(),
                    params: vec!["range_start".into(), "range_end".into(), "length".into()],
                }),
                f: builtins::i32_array,
            }
        ),
        (
            "u32_array".into(),
            Command::Builtin {
                params: Some(Params {
                    raw: "".into(),
                    params: vec!["range_start".into(), "range_end".into(), "length".into()],
                }),
                f: builtins::u32_array,
            }
        )
    ]);
}

pub fn cmd(args: &Vec<String>) -> String {
    let string = inside_quote(&args[0]);
    let cli = Cli::from_raw_args(&string);
    par_rngstr(&cli)
}

pub fn cmd_array(args: &Vec<String>) -> String {
    let string = inside_quote(&args[0]);
    let len: usize = args[1].parse().unwrap();
    let cli = Cli::from_raw_args(&string);
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
