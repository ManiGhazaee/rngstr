#[macro_export]
macro_rules! parse_args {
    ($args:expr, $($num:literal => $t:ty),+) => {
        ($($args[$num].parse::<$t>().unwrap()),+)
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
macro_rules! unfortunate {
    ($i:ident $j:ident $name:ident: [$ty:ty; _] = $value:expr) => {
        $id $name: [$ty; *$value.len()] = $value;
    }
}