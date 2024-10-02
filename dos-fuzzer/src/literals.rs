use std::collections::HashSet;
use std::fs;
use std::path::Path;

use syn::{visit::Visit, Lit};
use walkdir::WalkDir;

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
    let walkdir = WalkDir::new("../pulldown-cmark/src")
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .filter(|e| {
            if let Some(ext) = e.path().extension() {
                ext == "rs"
            } else {
                false
            }
        });

    let mut literal_parser = LiteralParser::new();

    let skipped_files = &[
        Path::new("../pulldown-cmark/src/entities.rs"),
        Path::new("../pulldown-cmark/src/main.rs"),
    ];
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
        .filter(|lit| !lit.contains(&b'\n') || lit.len() == 1)
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
        let content = fs::read_to_string(path).expect(&format!("unable to read file {:?}", path));
        let parsed = syn::parse_file(&content).expect(&format!("unable to parse file {:?}", path));
        self.visit_file(&parsed);
    }
}

impl<'ast> Visit<'ast> for LiteralParser {
    fn visit_lit(&mut self, lit: &'ast Lit) {
        if self.condition_depth == 0 && !self.in_static && !self.in_const {
            return;
        }
        match lit {
            Lit::Str(s) => drop(self.literals.insert(s.value().into_bytes())),
            Lit::ByteStr(s) => drop(self.literals.insert(s.value())),
            Lit::CStr(s) => drop(self.literals.insert(s.value().into_bytes())),
            Lit::Byte(b) => drop(self.literals.insert(vec![b.value()])),
            Lit::Char(c) => {
                let c = c.value();
                let mut v = vec![0; c.len_utf8()];
                c.encode_utf8(&mut v);
                self.literals.insert(v);
            }
            _ => (),
        }
    }

    fn visit_item_static(&mut self, item: &'ast syn::ItemStatic) {
        self.in_static = true;
        syn::visit::visit_item_static(self, item);
        self.in_static = false;
    }

    fn visit_item_const(&mut self, item: &'ast syn::ItemConst) {
        self.in_const = true;
        syn::visit::visit_item_const(self, item);
        self.in_const = false;
    }

    fn visit_expr_if(&mut self, expr: &'ast syn::ExprIf) {
        self.condition_depth += 1;
        self.visit_expr(&*expr.cond);
        self.condition_depth -= 1;
        self.visit_block(&expr.then_branch);
        if let Some((_, branch)) = &expr.else_branch {
            self.visit_expr(&*branch);
        }
    }

    fn visit_expr_while(&mut self, expr: &'ast syn::ExprWhile) {
        self.condition_depth += 1;
        self.visit_expr(&*expr.cond);
        self.condition_depth -= 1;
        self.visit_block(&expr.body);
    }

    fn visit_expr_for_loop(&mut self, expr: &'ast syn::ExprForLoop) {
        self.condition_depth += 1;
        self.visit_expr(&*expr.expr);
        self.condition_depth -= 1;
        self.visit_block(&expr.body);
    }

    fn visit_expr_match(&mut self, expr: &'ast syn::ExprMatch) {
        self.condition_depth += 1;
        self.visit_expr(&*expr.expr);
        self.condition_depth -= 1;
        for it in &expr.arms {
            self.visit_arm(it);
        }
    }

    fn visit_arm(&mut self, arm: &'ast syn::Arm) {
        self.visit_pat(&arm.pat);
        self.condition_depth += 1;
        if let Some((_, guard)) = &arm.guard {
            self.visit_expr(guard);
        }
        self.condition_depth -= 1;
        self.visit_expr(&*arm.body);
    }

    fn visit_pat(&mut self, pat: &'ast syn::Pat) {
        // Covers let-else patterns etc.
        self.condition_depth += 1;
        syn::visit::visit_pat(self, pat);
        self.condition_depth -= 1;
    }

    fn visit_attribute(&mut self, _attr: &'ast syn::Attribute) {
        // Ignore attributes, e.g. doc comments
    }
}
