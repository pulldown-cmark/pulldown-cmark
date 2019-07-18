use std::collections::HashSet;
use std::fs;
use std::path::Path;

use walkdir::WalkDir;
use syn::{Item, Expr, ImplItem, Stmt, ExprArray, ExprCall, ExprMethodCall, Visibility, Ident, Type,
          ExprTuple, Lit, Pat, punctuated::Punctuated, token::{Comma, Semi, Struct, Colon, Eq, Const, Brace},
          parse::{Parse, ParseStream, Error}, braced};
use proc_macro2::TokenStream;

/// Get all relevant literals from pulldown-cmark to generate fuzzing input from.
///
/// This method iterates over the source code of pulldown-cmark (except for `entities.rs` and `main.rs`.
/// It parses the source code an extracts all literals used in consts, statics, conditions and match
/// arm patterns and guards. Literals are Strs, ByteStrs, chars and bytes.
/// If an array is encountered, only the first element is extracted. We assume that all elements in
/// the array are used the same way to enter the same branch, like `if ["foo", "bar"].contains("baz")`.
///
/// Additionally, it manually adds some literals for uncovered edge-cases.
pub fn get() -> Vec<Vec<u8>> {
    // Get relevant literals from pulldown-cmark's source code.
    // See documentaiton of `literals`-module for more information.
    let walkdir = WalkDir::new("../src")
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .filter(|e| if let Some(ext) = e.path().extension() { ext == "rs" } else { false });

    let mut literal_parser = LiteralParser::new();

    let skipped_files = &[Path::new("../src/entities.rs"), Path::new("../src/main.rs")];
    for file in walkdir {
        if skipped_files.contains(&file.path()) {
            continue;
        }
        literal_parser.extract_literals_from_file(file.path());
    }

    let mut literals = literal_parser.into_literals();

    // Manually add literals.
    // ensure that the branch calling into `entities.rs` is covered
    literals.insert("&amp;".into());
    // Unicode no-break space
    literals.insert("\u{a0}".into());
    // 1/2/3/4-byte UTF-8 characters to ensure correct handling of unicode
    literals.insert("\u{0}".into());
    literals.insert("\u{80}".into());
    literals.insert("\u{800}".into());
    literals.insert("\u{10000}".into());

    literals
        .into_iter()
        .filter(|lit| !lit.contains(&b'\n'))
        .filter(|lit| !lit.is_empty())
        .collect()
}

#[derive(Debug, Default)]
struct LiteralParser {
    literals: HashSet<Vec<u8>>,
    // Whenever we enter an if-condition / match arm pattern, we increment this value.
    // Whenever we leave it again, we decrement it. This ensures correct behaviour when having a
    // condition inside a condition (which is to just extract everything).
    condition_depth: u32,
    in_static: bool,
    in_const: bool,
}

impl LiteralParser {
    pub fn new() -> LiteralParser {
        Default::default()
    }

    pub fn into_literals(self) -> HashSet<Vec<u8>> {
        assert_eq!(self.condition_depth, 0);
        self.literals
    }

    pub fn extract_literals_from_file<P: AsRef<Path>>(&mut self, path: P) {
        let path = path.as_ref();
        let content = fs::read_to_string(path)
            .expect(&format!("unable to read file {:?}", path));
        let parsed = syn::parse_file(&content)
            .expect(&format!("unable to parse file {:?}", path));
        self.extract_literals_from_items(parsed.items);
    }

    fn extract_literals_from_items(&mut self, items: Vec<Item>) {
        for item in items {
            self.extract_literals_from_item(item);
        }
    }

    fn extract_literals_from_item(&mut self, item: Item) {
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
            Item::Static(item) => {
                self.in_static = true;
                self.extract_literals_from_expr(*item.expr);
                self.in_static = false;
            },
            Item::Const(item) => {
                self.in_const = true;
                self.extract_literals_from_expr(*item.expr);
                self.in_const = false;
            },
            Item::Fn(item) => self.extract_literals_from_stmts(item.block.stmts),
            Item::Mod(item) => if let Some((_, item)) = item.content {
                self.extract_literals_from_items(item);
            },
            Item::Impl(item) => self.extract_literals_from_impl(item.items),
            Item::Macro(item) => self.extract_literals_from_macro(item.mac.tts),
            Item::Macro2(_) => unimplemented!("macros 2.0"),
        }
    }

    fn extract_literals_from_stmts(&mut self, stmts: Vec<Stmt>) {
        for stmt in stmts {
            match stmt {
                Stmt::Local(local) => if let Some((_, expr)) = local.init {
                    self.extract_literals_from_expr(*expr);
                },
                Stmt::Item(item) => self.extract_literals_from_item(item),
                Stmt::Expr(expr) | Stmt::Semi(expr, _) => self.extract_literals_from_expr(expr),
            }
        }
    }

    fn extract_literals_from_impl(&mut self, items: Vec<ImplItem>) {
        for item in items {
            match item {
                ImplItem::Const(item) => {
                    self.in_const = true;
                    self.extract_literals_from_expr(item.expr);
                    self.in_const = false;
                },
                ImplItem::Method(item) => self.extract_literals_from_stmts(item.block.stmts),
                ImplItem::Type(_)
                | ImplItem::Existential(_) => (),
                ImplItem::Macro(item) => self.extract_literals_from_macro(item.mac.tts),
                ImplItem::Verbatim(_) => unimplemented!("ImplItem::Verbatim"),
            }
        }
    }

    fn extract_literals_from_macro(&mut self, tts: TokenStream) {
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
                self.extract_literals_from_expr(expr);
            }
        } else if let Ok(args) = syn::parse2::<ExprArgsSemi>(tts.clone()) {
            for expr in args.args {
                self.extract_literals_from_expr(expr);
            }
        } else if let Ok(_) = syn::parse2::<Bitflags>(tts.clone()) {
            // ignore
        } else {
            unimplemented!("Unhandled macro invocation: {:?}", tts);
        }
    }

    fn extract_literals_from_lit(&mut self, lit: Lit) {
        if self.condition_depth == 0 && !self.in_static && !self.in_const {
            return;
        }
        match lit {
            Lit::Str(s) => drop(self.literals.insert(s.value().into_bytes())),
            Lit::ByteStr(s) => drop(self.literals.insert(s.value())),
            Lit::Byte(b) => drop(self.literals.insert(vec![b.value()])),
            Lit::Char(c) => {
                let c = c.value();
                let mut v = vec![0; c.len_utf8()];
                c.encode_utf8(&mut v);
                self.literals.insert(v);
            }
            Lit::Int(_) | Lit::Float(_) | Lit::Bool(_) | Lit::Verbatim(_) => (),
        }
    }


    fn extract_literals_from_expr(&mut self, expr: Expr) {
        match expr {
            Expr::Box(expr) => self.extract_literals_from_expr(*expr.expr),
            Expr::InPlace(expr) => self.extract_literals_from_expr(*expr.value),
            Expr::Array(ExprArray { elems, .. }) => {
                // If an array is used, we assume that all elements are equal.
                // For example in `if ["foo", "bar"].contains(baz) {` it's enough to just extract the
                // first element to cover that branch.
                if let Some(elem) = elems.into_iter().next() {
                    self.extract_literals_from_expr(elem);
                }
            }
            Expr::Tuple(ExprTuple { elems, .. }) => for expr in elems {
                self.extract_literals_from_expr(expr);
            },
            Expr::Call(ExprCall { func, args, .. })
            | Expr::MethodCall(ExprMethodCall { args, receiver: func, ..}) => {
                self.extract_literals_from_expr(*func);
                for arg in args {
                    self.extract_literals_from_expr(arg);
                }
            }
            Expr::Binary(expr) => {
                self.extract_literals_from_expr(*expr.left);
                self.extract_literals_from_expr(*expr.right);
            },
            Expr::Unary(expr) => self.extract_literals_from_expr(*expr.expr),
            Expr::Lit(expr) => self.extract_literals_from_lit(expr.lit),
            Expr::Cast(_) => (),
            Expr::Type(expr) => self.extract_literals_from_expr(*expr.expr),
            Expr::Let(expr) => {
                self.condition_depth += 1;
                for pat in expr.pats {
                    self.extract_literals_from_pat(pat);
                }
                self.condition_depth -= 1;
                self.extract_literals_from_expr(*expr.expr)
            },
            Expr::If(expr) => {
                self.condition_depth += 1;
                self.extract_literals_from_expr(*expr.cond);
                self.condition_depth -= 1;
                self.extract_literals_from_stmts(expr.then_branch.stmts);
                if let Some((_, expr)) = expr.else_branch {
                    self.extract_literals_from_expr(*expr)
                }
            },
            Expr::While(expr) => {
                self.condition_depth += 1;
                self.extract_literals_from_expr(*expr.cond);
                self.condition_depth -= 1;
                self.extract_literals_from_stmts(expr.body.stmts);
            },
            Expr::ForLoop(expr) => {
                self.condition_depth += 1;
                self.extract_literals_from_expr(*expr.expr);
                self.condition_depth -= 1;
                self.extract_literals_from_stmts(expr.body.stmts);
            },
            Expr::Loop(expr) => self.extract_literals_from_stmts(expr.body.stmts),
            Expr::Match(arm) => {
                self.condition_depth += 1;
                self.extract_literals_from_expr(*arm.expr);
                self.condition_depth -= 1;
                for arm in arm.arms {
                    self.condition_depth += 1;
                    for pat in arm.pats {
                        self.extract_literals_from_pat(pat);
                    }
                    if let Some((_, guard)) = arm.guard {
                        self.extract_literals_from_expr(*guard);
                    }
                    self.condition_depth -= 1;
                    self.extract_literals_from_expr(*arm.body);
                }
            },
            // TODO: are closure argument patterns relevant?
            Expr::Closure(expr) => self.extract_literals_from_expr(*expr.body),
            Expr::Unsafe(expr) => self.extract_literals_from_stmts(expr.block.stmts),
            Expr::Block(expr) => self.extract_literals_from_stmts(expr.block.stmts),
            Expr::Assign(expr) => {
                self.extract_literals_from_expr(*expr.left);
                self.extract_literals_from_expr(*expr.right);
            },
            Expr::AssignOp(expr) => {
                self.extract_literals_from_expr(*expr.left);
                self.extract_literals_from_expr(*expr.right);
            },
            Expr::Field(expr) => self.extract_literals_from_expr(*expr.base),
            Expr::Index(expr) => {
                self.extract_literals_from_expr(*expr.expr);
                self.extract_literals_from_expr(*expr.index);
            },
            // from is enough as we can trigger that range with the beginning
            Expr::Range(expr) => if let Some(val) = expr.from.or(expr.to) {
                self.extract_literals_from_expr(*val);
            },
            Expr::Path(_) => (),
            Expr::Reference(expr) => self.extract_literals_from_expr(*expr.expr),
            Expr::Break(expr) => if let Some(expr) = expr.expr {
                self.extract_literals_from_expr(*expr);
            },
            Expr::Continue(_) => (),
            Expr::Return(expr) => if let Some(expr) = expr.expr {
                self.extract_literals_from_expr(*expr);
            },
            Expr::Macro(expr) => self.extract_literals_from_macro(expr.mac.tts),
            Expr::Struct(expr) => {
                for field in expr.fields {
                    self.extract_literals_from_expr(field.expr);
                }
                if let Some(expr) = expr.rest {
                    self.extract_literals_from_expr(*expr);
                }
            },
            Expr::Repeat(expr) => {
                self.extract_literals_from_expr(*expr.expr);
                self.extract_literals_from_expr(*expr.len);
            },
            Expr::Paren(expr) => self.extract_literals_from_expr(*expr.expr),
            Expr::Group(expr) => self.extract_literals_from_expr(*expr.expr),
            Expr::Try(expr) => self.extract_literals_from_expr(*expr.expr),
            Expr::Async(expr) => self.extract_literals_from_stmts(expr.block.stmts),
            Expr::TryBlock(expr) => self.extract_literals_from_stmts(expr.block.stmts),
            Expr::Yield(expr) => if let Some(expr) = expr.expr {
                self.extract_literals_from_expr(*expr);
            },
            Expr::Verbatim(_) => unimplemented!("Expr::Verbatim"),
        }
    }

    fn extract_literals_from_pat(&mut self, pat: Pat) {
        match pat {
            Pat::Wild(_)
            | Pat::Path(_) => (),
            Pat::Ident(pat) => if let Some((_, pat)) = pat.subpat {
                self.extract_literals_from_pat(*pat);
            },
            Pat::Struct(pat) => for pat in pat.fields {
                self.extract_literals_from_pat(*pat.pat);
            },
            Pat::TupleStruct(pat) => for pat in pat.pat.front.into_iter().chain(pat.pat.back) {
                self.extract_literals_from_pat(pat);
            },
            Pat::Tuple(pat) => for pat in pat.front.into_iter().chain(pat.back) {
                self.extract_literals_from_pat(pat);
            },
            Pat::Box(pat) => self.extract_literals_from_pat(*pat.pat),
            Pat::Ref(pat) => self.extract_literals_from_pat(*pat.pat),
            Pat::Lit(pat) => self.extract_literals_from_expr(*pat.expr),
            // handling lo is enough as we can trigger that pattern with the lo element already
            Pat::Range(pat) => self.extract_literals_from_expr(*pat.lo),
            Pat::Slice(pat) => for pat in pat.front.into_iter().chain(pat.middle.map(|pat| *pat)).chain(pat.back) {
                self.extract_literals_from_pat(pat);
            },
            Pat::Macro(pat) => self.extract_literals_from_macro(pat.mac.tts),
            Pat::Verbatim(_) => unimplemented!("Pat::Verbatim"),
        }
    }
}
