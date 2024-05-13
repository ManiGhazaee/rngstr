#[macro_export]
macro_rules! parse_args {
    ($name:ident, $args:expr, $($num:literal => $t:ty),+) => {
        ($($args.get($num).ok_or(format!("expected argument '{}' at '{}'", $num.to_string(), stringify!($name)))?.parse::<$t>().map_err(|_| {"parsing".to_string()})?),+)
    };
}

#[macro_export]
macro_rules! gen_range_to_string_vec {
    ($start:ident..$end:ident, $len:expr) => {{
        let mut res = Vec::new();
        let mut rng = thread_rng();
        for _ in 0..$len {
            res.push(rng.gen_range($start..$end).to_string());
        }
        res
    }};
}

#[macro_export]
macro_rules! fn_types {
    ($($t:ident)+) => {
        $(pub fn $t(args: &Vec<String>) -> Result<String, String> {
            if args.get(0).is_some() && args.get(1).is_some() {
                let (start, end) = parse_args!($t, args,
                    0 => $t,
                    1 => $t
                );
                Ok(thread_rng().gen_range(start..end).to_string())
            } else {
                Ok(thread_rng().gen::<$t>().to_string())
            }

        })+
    };
}

#[macro_export]
macro_rules! builtin_types {
    ($($t:ident)+) => {
        [$(
            (
                stringify!($t).to_string(),
                Command::Builtin {
                    params: Some(Params {
                        raw: "".into(),
                        params: vec!["start".into(), "end".into()],
                    }),
                    f: builtins::$t,
                }
            ),
        )+]
    };
}
