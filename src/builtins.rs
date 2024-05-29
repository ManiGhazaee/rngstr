use super::*;

lazy_static! {
    pub static ref BUILTINS: Mutex<[(String, Proc); 20]> = {
        let mut x = [
            (
                "repeat".into(),
                Proc::Command {
                    params: Some(Params {
                        body: "--repeat count -s suffix".into(),
                        params: vec!["count".into(), "suffix".into()],
                    }),
                    config: Default::default(),
                },
            ),
            (
                "cmd".into(),
                Proc::Builtin {
                    params: Some(Params {
                        body: "".into(),
                        params: vec!["string".into()],
                    }),
                    f: builtins::cmd,
                },
            ),
            (
                "id".into(),
                Proc::Builtin {
                    params: Some(Params {
                        body: "".into(),
                        params: vec!["tag".into()],
                    }),
                    f: builtins::id,
                },
            ),
            (
                "group".into(),
                Proc::Builtin {
                    params: Some(Params {
                        body: "".into(),
                        params: vec!["items".into()],
                    }),
                    f: builtins::group,
                },
            ),
            (
                "array".into(),
                Proc::Macro {
                    params: Some(Params {
                        body: "[!repeat<length, \", \">(!procedure())]".into(),
                        params: vec!["procedure".into(), "length".into()],
                    }),
                    tokens: vec![],
                },
            ),
            (
                "array_fill".into(),
                Proc::Macro {
                    params: Some(Params {
                        body: "[!repeat<length, \", \">(element)]".into(),
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
    let cli = Config::from_body(&string);
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
    let cli = Config::from_body(&format!("--group {}", &args.join(" ")));
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
