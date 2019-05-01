use std::fs;
use std::path::Path;
use std::collections::HashSet;
use std::time::{Duration, Instant};
use std::panic;

use walkdir::WalkDir;
use syn::{Item, Expr, ImplItem, Stmt, ExprArray, ExprCall, ExprMethodCall, Visibility, Ident, Type,
    ExprTuple, Lit, Pat, punctuated::Punctuated, token::{Comma, Semi, Struct, Colon, Eq, Const, Brace},
    parse::{Parse, ParseStream, Error}, braced};
use proc_macro2::TokenStream;
use itertools::Itertools;
use pulldown_cmark::{Parser, Options};
use ndarray::{Array2, Array1};
use ndarray_stats::SummaryStatisticsExt;
use rand::seq::SliceRandom;
use rayon::iter::{ParallelIterator, IntoParallelIterator};

const BATCH_SIZE: usize = 1_000_000;
const COMBINATIONS: usize = 4;
const MAX_MILLIS: u128 = 500;
const MIN_MILLIS: u128 = 1;
const NUM_BYTES: usize = 256*1024;
const SAMPLE_SIZE: usize = 5;
const ACCEPTANCE_STDDEV: f64 = 0.5;


fn main() {
    let walkdir = WalkDir::new("../src")
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .filter(|e| if let Some(ext) = e.path().extension() { ext == "rs" } else { false });

    let mut literals = HashSet::new();
    for file in walkdir {
        if file.path() == Path::new("../src/entities.rs") {
            continue;
        }
        println!("parsing {}", file.path().display());
        let content = fs::read_to_string(file.path())
            .expect(&format!("unable to read file {:?}", file.path()));
        let parsed = syn::parse_file(&content)
            .expect(&format!("unable to parse file {:?}", file.path()));
        extract_literals_from_items(&mut literals, parsed.items);
    }

    let mut literals: Vec<_> = literals.into_iter()
        .filter(|lit| !lit.contains(&b'\n'))
        .filter(|lit| lit.len() != 0)
        .collect();
    literals.shuffle(&mut rand::thread_rng());

    let mut combs_iter = literals.iter().combinations(COMBINATIONS);
    let mut count = 0;
    loop {
        let combs: Vec<_> = (&mut combs_iter).take(BATCH_SIZE).collect();
        if combs.is_empty() {
            break;
        }
        count += combs.len();
        combs.into_par_iter().for_each(|comb| {
            let res = panic::catch_unwind(|| {
                let concatenated = comb.into_iter().flatten().cloned().collect::<Vec<_>>();
                let s = String::from_utf8(concatenated).unwrap();
                test(&s);
            });
            if let Err(e) = res {
                eprintln!("Panic during execution in thread:\n {:?}", e);
            }
        });
        println!("{}", count);
    }
}

fn test(pattern: &str) {
    let s = pattern.repeat(NUM_BYTES / pattern.len());
    let mut array = Array2::zeros((2, SAMPLE_SIZE));

    let mut iter = (1..=SAMPLE_SIZE).map(|i| {
        let mut n = s.len()/SAMPLE_SIZE*i;
        // find closest byte boundary
        while !s.is_char_boundary(n) {
            n += 1;
        }
        (test_single(&s[..n]), n)
    }).enumerate()
        .peekable();

    if (iter.peek().unwrap().1).0.as_millis() < MIN_MILLIS {
        return;
    }

    for (i, (dur, n)) in iter {
        array[[0, i]] = n as f64;
        array[[1, i]] = dur.as_micros() as f64;
        if dur.as_millis() > MAX_MILLIS {
            if i == 0 || i == 1 {
                print_find(pattern, &array, 0.0);
            }
            break;
        }
    }
    let slopes: Vec<_> = (0..SAMPLE_SIZE).tuple_combinations()
        .map(|(a, b)| {
            let x1 = array[[0, a]];
            let y1 = array[[1, a]];
            let x2 = array[[0, b]];
            let y2 = array[[1, b]];
            let dx = x2 - x1;
            let dy = y2 - y1;
            let slope = dy / dx;
            slope
        }).filter(|&slope| slope > 0.0)
        .collect();

    if slopes.is_empty() {
        return;
    }

    let slopes = Array1::from(slopes);
    let stddev = slopes.central_moment(2).unwrap().sqrt();

    //println!("{:<30}{}", stddev, pattern);
    //println!("{}", array);
    if stddev > ACCEPTANCE_STDDEV {
        print_find(pattern, &array, stddev);
    }
}

fn print_find(pattern: &str, array: &Array2<f64>, stddev: f64) {
    println!("
possible non-linear behaviour found
pattern: {:?}
{}
standard deviation: {}
\
    ", pattern, array.t(), stddev);
}

fn test_single(s: &str) -> Duration {
    let parser = Parser::new_ext(&s, Options::all());
    let time = Instant::now();
    parser.for_each(|evt| {
        black_box(evt);
    });
    time.elapsed()
}

fn extract_literals_from_items(literals: &mut HashSet<Vec<u8>>, items: Vec<Item>) {
    for item in items {
        extract_literals_from_item(literals, item);
    }
}

fn extract_literals_from_item(literals: &mut HashSet<Vec<u8>>, item: Item) {
    match item {
        Item::ExternCrate(_)
        | Item::Use(_)
        | Item::ForeignMod(_)
        | Item::Type(_)
        | Item::Existential(_)
        | Item::Struct(_)
        | Item::Enum(_)
        | Item::Union(_)
        | Item::Trait(_)
        | Item::TraitAlias(_)
        | Item::Verbatim(_) => (),
        Item::Static(item) => extract_literals_from_expr(literals, *item.expr),
        Item::Const(item) => extract_literals_from_expr(literals, *item.expr),
        Item::Fn(item) => extract_literals_from_stmts(literals, item.block.stmts),
        Item::Mod(item) => if let Some((_, item)) = item.content {
            extract_literals_from_items(literals, item);
        },
        Item::Impl(item) => extract_literals_from_impl(literals, item.items),
        Item::Macro(item) => extract_literals_from_macro(literals, item.mac.tts),
        Item::Macro2(_) => unimplemented!("macros 2.0"),
    }
}

fn extract_literals_from_stmts(literals: &mut HashSet<Vec<u8>>, stmts: Vec<Stmt>) {
    for stmt in stmts {
        match stmt {
            Stmt::Local(local) => if let Some((_, expr)) = local.init {
                extract_literals_from_expr(literals, *expr);
            },
            Stmt::Item(item) => extract_literals_from_item(literals, item),
            Stmt::Expr(expr) | Stmt::Semi(expr, _) => extract_literals_from_expr(literals, expr),
        }
    }
}

fn extract_literals_from_impl(literals: &mut HashSet<Vec<u8>>, items: Vec<ImplItem>) {
    for item in items {
        match item {
            ImplItem::Const(item) => extract_literals_from_expr(literals, item.expr),
            ImplItem::Method(item) => extract_literals_from_stmts(literals, item.block.stmts),
            ImplItem::Type(_)
            | ImplItem::Existential(_) => (),
            ImplItem::Macro(item) => extract_literals_from_macro(literals, item.mac.tts),
            ImplItem::Verbatim(_) => unimplemented!("ImplItem::Verbatim"),
        }
    }
}

fn extract_literals_from_macro(literals: &mut HashSet<Vec<u8>>, tts: TokenStream) {
    macro_rules! args {
        ($name:ident, $delim:ty) => {
            struct $name {
                args: Punctuated<Expr, $delim>,
            }
            impl Parse for $name {
                fn parse(input: ParseStream) -> Result<Self, Error> {
                    Ok($name {
                        args: input.parse_terminated(Expr::parse)?,
                    })
                }
            }
        }
    }
    args!(ExprArgsComma, Comma);
    args!(ExprArgsSemi, Semi);
    #[allow(dead_code)]
    struct BitflagsField {
        const_token: Const,
        ident: Ident,
        eq_token: Eq,
        expr: Expr,
    }
    impl Parse for BitflagsField {
        fn parse(input: ParseStream) -> Result<Self, Error> {
            Ok(BitflagsField {
                const_token: input.parse().unwrap(),
                ident: input.parse().unwrap(),
                eq_token: input.parse().unwrap(),
                expr: input.parse().unwrap(),
            })
        }
    }
    #[allow(dead_code)]
    struct Bitflags {
        vis: Visibility,
        struct_token: Struct,
        ident: Ident,
        colon: Colon,
        repr_type: Type,
        brace_token: Brace,
        fields: Punctuated<BitflagsField, Semi>,
    }
    impl Parse for Bitflags {
        fn parse(input: ParseStream) -> Result<Self, Error> {
            let content;
            Ok(Bitflags {
                vis: input.parse().unwrap(),
                struct_token: input.parse().unwrap(),
                ident: input.parse().unwrap(),
                colon: input.parse().unwrap(),
                repr_type: input.parse().unwrap(),
                brace_token: braced!(content in input),
                fields: content.parse_terminated(BitflagsField::parse).unwrap(),
            })
        }
    }

    if let Ok(args) = syn::parse2::<ExprArgsComma>(tts.clone()) {
        for expr in args.args {
            extract_literals_from_expr(literals, expr);
        }
    } else if let Ok(args) = syn::parse2::<ExprArgsSemi>(tts.clone()) {
        for expr in args.args {
            extract_literals_from_expr(literals, expr);
        }
    } else if let Ok(_) = syn::parse2::<Bitflags>(tts.clone()) {
        // ignore
    } else {
        unimplemented!("Unhandled macro invocation: {:?}", tts);
    }
}

fn extract_literals_from_lit(literals: &mut HashSet<Vec<u8>>, lit: Lit) {
    match lit {
        Lit::Str(s) => drop(literals.insert(s.value().into_bytes())),
        Lit::ByteStr(s) => drop(literals.insert(s.value())),
        Lit::Byte(b) => drop(literals.insert(vec![b.value()])),
        Lit::Char(c) => {
            let c = c.value();
            let mut v = vec![0; c.len_utf8()];
            c.encode_utf8(&mut v);
            literals.insert(v);
        }
        Lit::Int(_) | Lit::Float(_) | Lit::Bool(_) | Lit::Verbatim(_) => (),
    }
}

fn extract_literals_from_expr(literals: &mut HashSet<Vec<u8>>, expr: Expr) {
    match expr {
        Expr::Box(expr) => extract_literals_from_expr(literals, *expr.expr),
        Expr::InPlace(expr) => extract_literals_from_expr(literals, *expr.value),
        Expr::Array(ExprArray { elems: exprs, .. })
        | Expr::Call(ExprCall { args: exprs, .. })
        | Expr::MethodCall(ExprMethodCall { args: exprs, ..})
        | Expr::Tuple(ExprTuple { elems: exprs, .. }) => for expr in exprs {
            extract_literals_from_expr(literals, expr);
        },
        Expr::Binary(expr) => {
            extract_literals_from_expr(literals, *expr.left);
            extract_literals_from_expr(literals, *expr.right);
        },
        Expr::Unary(expr) => extract_literals_from_expr(literals, *expr.expr),
        Expr::Lit(expr) => extract_literals_from_lit(literals, expr.lit),
        Expr::Cast(_) => (),
        Expr::Type(expr) => extract_literals_from_expr(literals, *expr.expr),
        Expr::Let(expr) => extract_literals_from_expr(literals, *expr.expr),
        Expr::If(expr) => {
            extract_literals_from_expr(literals, *expr.cond);
            extract_literals_from_stmts(literals, expr.then_branch.stmts);
            if let Some((_, expr)) = expr.else_branch {
                extract_literals_from_expr(literals, *expr)
            }
        },
        Expr::While(expr) => {
            extract_literals_from_expr(literals, *expr.cond);
            extract_literals_from_stmts(literals, expr.body.stmts);
        },
        Expr::ForLoop(expr) => {
            extract_literals_from_expr(literals, *expr.expr);
            extract_literals_from_stmts(literals, expr.body.stmts);
        },
        Expr::Loop(expr) => extract_literals_from_stmts(literals, expr.body.stmts),
        Expr::Match(expr) => {
            extract_literals_from_expr(literals, *expr.expr);
            for arm in expr.arms {
                for pat in arm.pats {
                    extract_literals_from_pat(literals, pat);
                }
                extract_literals_from_expr(literals, *arm.body);
            }
        },
        // TODO: are closure argument patterns relevant?
        Expr::Closure(expr) => extract_literals_from_expr(literals, *expr.body),
        Expr::Unsafe(expr) => extract_literals_from_stmts(literals, expr.block.stmts),
        Expr::Block(expr) => extract_literals_from_stmts(literals, expr.block.stmts),
        Expr::Assign(expr) => {
            extract_literals_from_expr(literals, *expr.left);
            extract_literals_from_expr(literals, *expr.right);
        },
        Expr::AssignOp(expr) => {
            extract_literals_from_expr(literals, *expr.left);
            extract_literals_from_expr(literals, *expr.right);
        },
        Expr::Field(expr) => extract_literals_from_expr(literals, *expr.base),
        Expr::Index(expr) => {
            extract_literals_from_expr(literals, *expr.expr);
            extract_literals_from_expr(literals, *expr.index);
        },
        // from is enough as we can trigger that range with the beginning
        Expr::Range(expr) => if let Some(val) = expr.from.or(expr.to) {
            extract_literals_from_expr(literals, *val);
        },
        Expr::Path(_) => (),
        Expr::Reference(expr) => extract_literals_from_expr(literals, *expr.expr),
        Expr::Break(expr) => if let Some(expr) = expr.expr {
            extract_literals_from_expr(literals, *expr);
        },
        Expr::Continue(_) => (),
        Expr::Return(expr) => if let Some(expr) = expr.expr {
            extract_literals_from_expr(literals, *expr);
        },
        Expr::Macro(expr) => extract_literals_from_macro(literals, expr.mac.tts),
        Expr::Struct(expr) => {
            for field in expr.fields {
                extract_literals_from_expr(literals, field.expr);
            }
            if let Some(expr) = expr.rest {
                extract_literals_from_expr(literals, *expr);
            }
        },
        Expr::Repeat(expr) => {
            extract_literals_from_expr(literals, *expr.expr);
            extract_literals_from_expr(literals, *expr.len);
        },
        Expr::Paren(expr) => extract_literals_from_expr(literals, *expr.expr),
        Expr::Group(expr) => extract_literals_from_expr(literals, *expr.expr),
        Expr::Try(expr) => extract_literals_from_expr(literals, *expr.expr),
        Expr::Async(expr) => extract_literals_from_stmts(literals, expr.block.stmts),
        Expr::TryBlock(expr) => extract_literals_from_stmts(literals, expr.block.stmts),
        Expr::Yield(expr) => if let Some(expr) = expr.expr {
            extract_literals_from_expr(literals, *expr);
        },
        Expr::Verbatim(_) => unimplemented!("Expr::Verbatim"),
    }
}

fn extract_literals_from_pat(literals: &mut HashSet<Vec<u8>>, pat: Pat) {
    match pat {
        Pat::Wild(_)
        | Pat::Path(_) => (),
        Pat::Ident(pat) => if let Some((_, pat)) = pat.subpat {
            extract_literals_from_pat(literals, *pat);
        },
        Pat::Struct(pat) => for pat in pat.fields {
            extract_literals_from_pat(literals, *pat.pat);
        },
        Pat::TupleStruct(pat) => for pat in pat.pat.front.into_iter().chain(pat.pat.back) {
            extract_literals_from_pat(literals, pat);
        },
        Pat::Tuple(pat) => for pat in pat.front.into_iter().chain(pat.back) {
            extract_literals_from_pat(literals, pat);
        },
        Pat::Box(pat) => extract_literals_from_pat(literals, *pat.pat),
        Pat::Ref(pat) => extract_literals_from_pat(literals, *pat.pat),
        Pat::Lit(pat) => extract_literals_from_expr(literals, *pat.expr),
        // handling lo is enough as we can trigger that pattern with the lo element already
        Pat::Range(pat) => extract_literals_from_expr(literals, *pat.lo),
        Pat::Slice(pat) => for pat in pat.front.into_iter().chain(pat.middle.map(|pat| *pat)).chain(pat.back) {
            extract_literals_from_pat(literals, pat);
        },
        Pat::Macro(pat) => extract_literals_from_macro(literals, pat.mac.tts),
        Pat::Verbatim(_) => unimplemented!("Pat::Verbatim"),
    }
}

// stolen from criterion
// https://github.com/bheisler/criterion.rs/blob/c21de8397a1e879321ab4ffbf3d5a6b0997300c3/src/lib.rs#L157
pub fn black_box<T>(dummy: T) -> T {
    unsafe {
        let ret = std::ptr::read_volatile(&dummy);
        std::mem::forget(dummy);
        ret
    }
}
