use super::*;

lazy_static! {
    pub static ref BUILTINS: Mutex<[(String, Command); 20]> = {
        let mut x = [
            (
                "repeat".into(),
                Command::Command {
                    params: Some(Params {
                        raw: "--repeat count -s suffix".into(),
                        params: vec!["count".into(), "suffix".into()],
                    }),
                    cli: Default::default(),
                },
            ),
            (
                "cmd".into(),
                Command::Builtin {
                    params: Some(Params {
                        raw: "".into(),
                        params: vec!["string".into()],
                    }),
                    f: builtins::cmd,
                },
            ),
            (
                "id".into(),
                Command::Builtin {
                    params: Some(Params {
                        raw: "".into(),
                        params: vec!["tag".into()],
                    }),
                    f: builtins::id,
                },
            ),
            (
                "group".into(),
                Command::Builtin {
                    params: Some(Params {
                        raw: "".into(),
                        params: vec!["items".into()],
                    }),
                    f: builtins::group,
                },
            ),
            (
                "array".into(),
                Command::Macro {
                    params: Some(Params {
                        raw: "[!repeat<length, \", \">(!command())]".into(),
                        params: vec!["command".into(), "length".into()],
                    }),
                    tokens: vec![],
                },
            ),
            (
                "array_fill".into(),
                Command::Macro {
                    params: Some(Params {
                        raw: "[!repeat<length, \", \">(element)]".into(),
                        params: vec!["element".into(), "length".into()],
                    }),
                    tokens: vec![],
                },
            ),
        ];
        let mut y = builtin_types! { u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 f32 f64 usize isize };
        let x = concat(&mut x, &mut y);

        Mutex::new(x)
    };
}

pub fn cmd(args: &Vec<String>) -> Result<String, String> {
    let string = inside_quote(&args[0]);
    let cli = Cli::from_raw_args(&string);
    Ok(par_rngstr(&cli))
}

pub fn id(args: &Vec<String>) -> Result<String, String> {
    let tag = parse_args!(id, args, 0 => String);
    let mut lock = IDS.lock().unwrap();
    if let Some(t) = lock.get_mut(&tag) {
        let ret = *t;
        *t += 1;
        Ok(ret.to_string())
    } else {
        lock.insert(tag, 1);
        Ok(0.to_string())
    }
}

pub fn group(args: &Vec<String>) -> Result<String, String> {
    let cli = Cli::from_raw_args(&format!("--group {}", &args.join(" ")));
    Ok(par_rngstr(&cli))
}

fn concat<T: Default, const A: usize, const B: usize, const C: usize>(
    a: &mut [T; A],
    b: &mut [T; B],
) -> [T; C] {
    assert_eq!(A + B, C, "C len should be {}", A + B);
    core::array::from_fn(|i| {
        if i < A {
            std::mem::take(&mut a[i])
        } else {
            std::mem::take(&mut b[i - A])
        }
    })
}

fn_types! { u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 f32 f64 usize isize }
