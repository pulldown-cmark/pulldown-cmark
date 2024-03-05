//! Fuzzin helper functions.

use std::collections::HashMap;
use std::convert::TryInto;
use std::process::{Child, Command};
use std::ptr;

use anyhow::anyhow;

use mozjs::jsapi::{EnterRealm, HandleValueArray, LeaveRealm, JS_NewGlobalObject, OnNewGlobalHookOption};
use mozjs::jsval::UndefinedValue;
use mozjs::rooted;
use mozjs::rust::SIMPLE_GLOBAL_CLASS;
use mozjs::rust::{JSEngine, RealmOptions, Runtime};
use mozjs::rust::wrappers::JS_CallFunctionName;
use mozjs::conversions::ToJSValConvertible;

use pulldown_cmark::{CodeBlockKind, Event, LinkType, Parser, Tag, TagEnd};

use quick_xml::escape::unescape;
use quick_xml::events::Event as XmlEvent;
use quick_xml::reader::Reader;

use reqwest::blocking as rq;

use serde::{Serialize, Deserialize};

fn urldecode(data: &str) -> String {
    let decoded = urlencoding::decode_binary(data.as_bytes());
    urlencoding::encode_binary(&decoded[..]).to_string()
}

/// Send Markdown `text` to `pulldown-cmark` and return Markdown
/// events.
pub fn pulldown_cmark(text: &str) -> Vec<Event<'_>> {
    Parser::new(text).collect()
}

/// Send Markdown `text` to `pulldown-cmark` and return Markdown
/// events.
pub fn pulldown_cmark_ext(text: &str) -> Vec<Event<'_>> {
    Parser::new_ext(text, pulldown_cmark::Options::ENABLE_MATH).collect()
}

pub struct PandocHandle {
    child: Child,
    client: rq::Client,
    port: u16,
}

impl PandocHandle {
    pub fn new() -> PandocHandle {
        let _ = Command::new("pandoc-server")
            .args(["--version"])
            .output()
            .expect("failed to run pandoc-server: is it installed?");
        let mut port: Option<u16> = None;
        let mut child: Option<Child> = None;
        let mut stashed_error = None;
        for try_port in 3333..3344 {
            let try_port_string = try_port.to_string();
            match Command::new("pandoc-server")
                .args(["--port", &try_port_string])
                .spawn()
            {
                Ok(try_server) => {
                    child = Some(try_server);
                    port = Some(try_port);
                    stashed_error = None;
                    break;
                }
                Err(e) => {
                    stashed_error = Some(e);
                    println!("{try_port}");
                }
            }
        }
        if let Some(error) = stashed_error {
            panic!("failed to launch pandoc server: {:?}", error);
        }
        PandocHandle {
            port: port.expect("failed to launch pandoc server: [[no port]]"),
            child: child.expect("failed to launch pandoc server: [[no child]]"),
            client: rq::Client::new(),
        }
    }
    pub fn get_ast(&self, text: &str) -> anyhow::Result<PandocAst> {
        let mut i = 0;
        loop {
            let result = self.get_ast_(text);
            if i > 6 || result.is_ok() || result.as_ref().unwrap_err().to_string().contains("recursion limit") {
                return result;
            }
            i += 1;
            println!("retry");
            std::thread::sleep(std::time::Duration::from_secs(i));
        }
    }
    fn get_ast_(&self, text: &str) -> anyhow::Result<PandocAst> {
        let PandocHandle { client, port, .. } = self;
        let request_json = serde_json::to_string(&PandocRequest {
            text,
            from: "commonmark+tex_math_dollars",
            to: "json",
        })?;
        let res = client.post(&format!("http://localhost:{port}"))
            .body(request_json)
            .header(reqwest::header::ACCEPT, "application/octet-stream")
            .header(reqwest::header::CONTENT_TYPE, "application/json")
            .send()?;
        let status = res.status();
        if status != reqwest::StatusCode::OK {
            Err(anyhow!("got failed status from pandoc: {status}"))?;
        }
        Ok(res.json()?)
    }
}

impl Drop for PandocHandle {
    fn drop(&mut self) {
        let mut n = 0;
        while let Err(_) = self.child.wait() {
            std::thread::sleep(std::time::Duration::from_secs(n*n));
            n += 1;
            if n > 10 {
                break;
            }
        }
    }
}

/// <https://github.com/jgm/pandoc/blob/main/doc/pandoc-server.md>
#[derive(Serialize)]
struct PandocRequest<'a> {
    text: &'a str,
    from: &'a str,
    to: &'a str,
}

pub mod pandoc_ast {
    use anyhow::anyhow;
    use pulldown_cmark::{CowStr, Event, Tag, TagEnd, CodeBlockKind, HeadingLevel};
    use std::collections::HashMap;
    use std::convert::TryFrom;
    use std::iter::{self, FromIterator};
    use serde::Deserialize;

    #[derive(Debug, Deserialize)]
    pub struct Pandoc {
        #[allow(dead_code)]
        meta: HashMap<String, serde_json::Value>,
        blocks: Vec<Block>,
    }

    impl Pandoc {
        pub fn to_events(&self) -> anyhow::Result<Vec<Event<'static>>> {
            let v: anyhow::Result<Vec<Vec<Event::<'static>>>> = Result::from_iter(
                self.blocks.iter()
                    .map(|b| b.to_events(&mut 1))
            );
            Ok(v?.into_iter().flat_map(|v| v).collect())
        }
    }
    
    #[derive(Debug, Deserialize)]
    #[serde(tag = "t", content = "c")]
    enum Block {
        Para(Vec<Inline>),
        Plain(Vec<Inline>),
        LineBlock(Vec<Vec<Inline>>),
        CodeBlock(Attr, String),
        RawBlock(String, String),
        BlockQuote(Vec<Block>),
        OrderedList(ListAttributes, Vec<Vec<Block>>),
        BulletList(Vec<Vec<Block>>),
        DefinitionList(Vec<(Vec<Inline>, Vec<Vec<Block>>)>),
        Header(usize, Attr, Vec<Inline>),
        HorizontalRule,
        Table(Box<(Attr, Caption, Vec<ColSpec>, TableHead, Vec<TableBody>, TableFoot)>),
        Figure(Attr, Caption, Vec<Block>),
        Div(Attr, Vec<Block>),
    }

    impl Block {
        fn to_events(&self, footnote_number: &mut u64) -> anyhow::Result<Vec<Event<'static>>> {
            use Block::*;
            Ok(match self {
                Para(inlines) => {
                    let v: anyhow::Result<Vec<Vec<Event::<'static>>>> = Result::from_iter(
                        inlines.iter()
                            .map(|b| b.to_events(footnote_number))
                    );
                    iter::once(Event::Start(Tag::Paragraph))
                        .chain(v?.into_iter().flat_map(|v| v))
                        .chain(iter::once(Event::End(TagEnd::Paragraph)))
                        .collect()
                }
                Plain(inlines) => {
                    let v: anyhow::Result<Vec<Vec<Event::<'static>>>> = Result::from_iter(
                        inlines.iter()
                            .map(|b| b.to_events(footnote_number))
                    );
                    v?.into_iter().flat_map(|v| v).collect()
                }
                LineBlock(_) => Err(anyhow!("pulldown-cmark does not support line blocks"))?,
                CodeBlock(_attr, string) => {
                    // Pandoc's AST doesn't distinguish between indented and fenced code blocks.
                    // Emit them all as indented, and ignore the lang string.
                    iter::once(Event::Start(Tag::CodeBlock(CodeBlockKind::Indented)))
                        .chain(iter::once(Event::Text(CowStr::Boxed(string[..].into()))))
                        .chain(iter::once(Event::End(TagEnd::CodeBlock)))
                        .collect()
                },
                RawBlock(x, html) if x == "html" || x == "HTML" => {
                    vec![
                        Event::Start(Tag::HtmlBlock),
                        Event::Html(CowStr::Boxed(html[..].into())),
                        Event::End(TagEnd::HtmlBlock),
                    ]
                },
                RawBlock(_, _) => Err(anyhow!("pulldown-cmark does not support raw blocks"))?,
                BlockQuote(blocks) => {
                    let v: anyhow::Result<Vec<Vec<Event::<'static>>>> = Result::from_iter(
                        blocks.iter()
                            .map(|b| b.to_events(footnote_number))
                    );
                    iter::once(Event::Start(Tag::BlockQuote))
                        .chain(v?.into_iter().flat_map(|v| v))
                        .chain(iter::once(Event::End(TagEnd::BlockQuote)))
                        .collect()
                }
                OrderedList(ListAttributes(start, _number_style, _number_delim), items) => {
                    let v: anyhow::Result<Vec<Vec<Event::<'static>>>> = Result::from_iter(
                        items.into_iter()
                            .map(|item| {
                                let v: Vec<anyhow::Result<Vec<Event>>> = item
                                    .iter()
                                    .map(|b| b.to_events(footnote_number))
                                    .collect();
                                let v: anyhow::Result<Vec<Vec<Event>>> = Result::from_iter(v.into_iter());
                                match v {
                                    Ok(v) => {
                                        let mut v: Vec<Event> = v.into_iter().flat_map(|v| v).collect();
                                        v.insert(0, Event::Start(Tag::Item));
                                        v.push(Event::End(TagEnd::Item));
                                        Ok(v)
                                    }
                                    Err(e) => Err(e),
                                }
                            })
                    );
                    iter::once(Event::Start(Tag::List(Some(*start))))
                        .chain(v?.into_iter().flat_map(|v| v))
                        .chain(iter::once(Event::End(TagEnd::List(true))))
                        .collect()
                }
                BulletList(items) => {
                    let v: anyhow::Result<Vec<Vec<Event::<'static>>>> = Result::from_iter(
                        items.into_iter()
                            .map(|item| {
                                let v: Vec<anyhow::Result<Vec<Event>>> = item
                                    .iter()
                                    .map(|b| b.to_events(footnote_number))
                                    .collect();
                                let v: anyhow::Result<Vec<Vec<Event>>> = Result::from_iter(v.into_iter());
                                match v {
                                    Ok(v) => {
                                        let mut v: Vec<Event> = v.into_iter().flat_map(|v| v).collect();
                                        v.insert(0, Event::Start(Tag::Item));
                                        v.push(Event::End(TagEnd::Item));
                                        Ok(v)
                                    }
                                    Err(e) => Err(e),
                                }
                            })
                    );
                    iter::once(Event::Start(Tag::List(None)))
                        .chain(v?.into_iter().flat_map(|v| v))
                        .chain(iter::once(Event::End(TagEnd::List(false))))
                        .collect()
                }
                DefinitionList(..) => Err(anyhow!("pulldown-cmark does not support definition lists"))?,
                Header(level, _attr, inlines) => {
                    let level = HeadingLevel::try_from(*level).map_err(|_| anyhow!("invalid heading level"))?;
                    let v: anyhow::Result<Vec<Vec<Event::<'static>>>> = Result::from_iter(
                        inlines.iter()
                            .map(|b| b.to_events(footnote_number))
                    );
                    iter::once(Event::Start(Tag::Heading { level, id: None, classes: vec![], attrs: vec![] }))
                        .chain(v?.into_iter().flat_map(|v| v))
                        .chain(iter::once(Event::End(TagEnd::Heading(level))))
                        .collect()
                }
                HorizontalRule => vec![Event::Rule],
                Table(b) => {
                    let (_attr, _caption, colspecs, head, body, foot) = &**b;
                    let alignment = colspecs
                        .iter()
                        .map(|ColSpec(alignment, _colwidth)| match alignment {
                            Alignment::AlignLeft => pulldown_cmark::Alignment::Left,
                            Alignment::AlignRight => pulldown_cmark::Alignment::Right,
                            Alignment::AlignCenter => pulldown_cmark::Alignment::Center,
                            Alignment::AlignDefault => pulldown_cmark::Alignment::None,
                        })
                        .collect();
                    let mut out = Vec::new();
                    out.push(Event::Start(Tag::Table(alignment)));
                    for row in head.1.iter() {
                        let v: anyhow::Result<Vec<Vec<Event::<'static>>>> = Result::from_iter(
                            row.1.iter()
                                .flat_map(|cell| {
                                    let mut c = cell.4.iter().map(|b| b.to_events(footnote_number)).collect::<Vec<_>>();
                                    c.insert(0, Ok(vec![Event::Start(Tag::TableCell)]));
                                    c.push(Ok(vec![Event::End(TagEnd::TableCell)]));
                                    c
                                })
                        );
                        out.push(Event::Start(Tag::TableHead));
                        out.extend(v?.into_iter().flat_map(|v| v));
                        out.push(Event::End(TagEnd::TableHead));
                    }
                    for body in body {
                        if !body.2.is_empty() {
                            Err(anyhow!("intermediate table headers not supported"))?;
                        }
                        for row in body.3.iter() {
                            let v: anyhow::Result<Vec<Vec<Event::<'static>>>> = Result::from_iter(
                                row.1.iter()
                                    .flat_map(|cell| {
                                        let mut c = cell.4.iter().map(|b| b.to_events(footnote_number)).collect::<Vec<_>>();
                                        c.insert(0, Ok(vec![Event::Start(Tag::TableCell)]));
                                        c.push(Ok(vec![Event::End(TagEnd::TableCell)]));
                                        c
                                    })
                            );
                            out.push(Event::Start(Tag::TableRow));
                            out.extend(v?.into_iter().flat_map(|v| v));
                            out.push(Event::End(TagEnd::TableRow));
                        }
                    }
                    if !foot.1.is_empty() {
                        Err(anyhow!("table footers not supported"))?;
                    }
                    out.push(Event::End(TagEnd::Table));
                    out
                }
                Figure(..) => Err(anyhow!("pulldown-cmark does not support figures"))?,
                Div(..) => Err(anyhow!("pulldown-cmark does not support divs"))?,
            })
        }
    }
    
    #[derive(Debug, Deserialize)]
    #[serde(tag = "t", content = "c")]
    enum Inline {
        Str(String),
        Emph(Vec<Inline>),
        Underline(Vec<Inline>),
        Strong(Vec<Inline>),
        Strikeout(Vec<Inline>),
        Superscript(Vec<Inline>),
        Subscript(Vec<Inline>),
        Smallcaps(Vec<Inline>),
        Quoted(Box<(QuoteType, Vec<Inline>)>),
        Cite(Box<(Vec<serde_json::Value>, Vec<Inline>)>),
        Code(Attr, String),
        Space,
        SoftBreak,
        LineBreak,
        Math(Box<(MathKind, String)>),
        RawInline(String, String),
        Link(Box<(Attr, Vec<Inline>, Target)>),
        Image(Box<(Attr, Vec<Inline>, Target)>),
        Note(Vec<Block>),
        Span(Attr, String),
    }

    #[derive(Debug, Deserialize)]
    #[serde(tag = "t", content = "c")]
    enum MathKind {
        InlineMath,
        DisplayMath,
    }

    impl Inline {
        fn to_events(&self, footnote_number: &mut u64) -> anyhow::Result<Vec<Event<'static>>> {
            use Inline::*;
            Ok(match self {
                Str(string) => vec![Event::Text(CowStr::Boxed(string[..].into()))],
                Emph(inlines) => {
                    let v: anyhow::Result<Vec<Vec<Event::<'static>>>> = Result::from_iter(
                        inlines.iter()
                            .map(|b| b.to_events(footnote_number))
                    );
                    iter::once(Event::Start(Tag::Emphasis))
                        .chain(v?.into_iter().flat_map(|v| v))
                        .chain(iter::once(Event::End(TagEnd::Emphasis)))
                        .collect()
                }
                Underline(inlines) => {
                    let v: anyhow::Result<Vec<Vec<Event::<'static>>>> = Result::from_iter(
                        inlines.iter()
                            .map(|b| b.to_events(footnote_number))
                    );
                    iter::once(Event::Start(Tag::Emphasis))
                        .chain(v?.into_iter().flat_map(|v| v))
                        .chain(iter::once(Event::End(TagEnd::Emphasis)))
                        .collect()
                }
                Strong(inlines) => {
                    let v: anyhow::Result<Vec<Vec<Event::<'static>>>> = Result::from_iter(
                        inlines.iter()
                            .map(|b| b.to_events(footnote_number))
                    );
                    iter::once(Event::Start(Tag::Strong))
                        .chain(v?.into_iter().flat_map(|v| v))
                        .chain(iter::once(Event::End(TagEnd::Strong)))
                        .collect()
                }
                Strikeout(inlines) => {
                    let v: anyhow::Result<Vec<Vec<Event::<'static>>>> = Result::from_iter(
                        inlines.iter()
                            .map(|b| b.to_events(footnote_number))
                    );
                    iter::once(Event::Start(Tag::Strikethrough))
                        .chain(v?.into_iter().flat_map(|v| v))
                        .chain(iter::once(Event::End(TagEnd::Strikethrough)))
                        .collect()
                }
                Superscript(..) => Err(anyhow!("pulldown-cmark does not support superscript"))?,
                Subscript(..) => Err(anyhow!("pulldown-cmark does not support subscript"))?,
                Smallcaps(..) => Err(anyhow!("pulldown-cmark does not support smallcaps"))?,
                Quoted(..) => Err(anyhow!("pulldown-cmark does not support inline quotes"))?,
                Cite(..) => Err(anyhow!("pulldown-cmark does not support citations"))?,
                Code(_attr, text) => vec![Event::Code(CowStr::Boxed(text[..].into()))],
                Space => vec![Event::Text(" ".into())],
                SoftBreak => vec![Event::SoftBreak],
                LineBreak => vec![Event::HardBreak],
                Math(inner) => {
                    let (kind, text) = &**inner;
                    vec![match kind {
                        MathKind::DisplayMath => Event::DisplayMath(text[..].to_string().into()),
                        MathKind::InlineMath => Event::InlineMath(text[..].to_string().into()),
                    }]
                },
                RawInline(x, html) if x == "html" || x == "HTML" => {
                    iter::once(Event::InlineHtml(CowStr::Boxed(html[..].into())))
                        .collect()
                },
                RawInline(..) => Err(anyhow!("pulldown-cmark does not support non-html raw code"))?,
                Link(b) => {
                    let (_attr, inlines, Target(url, title)) = &**b;
                    let v: anyhow::Result<Vec<Vec<Event::<'static>>>> = Result::from_iter(
                        inlines.iter()
                            .map(|b| b.to_events(footnote_number))
                    );
                    // Pandoc AST does not track the type of link.
                    iter::once(Event::Start(Tag::Link { link_type: pulldown_cmark::LinkType::Inline, dest_url: CowStr::Boxed(url[..].into()), title: CowStr::Boxed(title[..].into()), id: "".into() }))
                        .chain(v?.into_iter().flat_map(|v| v))
                        .chain(iter::once(Event::End(TagEnd::Link)))
                        .collect()
                }
                Image(b) => {
                    let (_attr, inlines, Target(url, title)) = &**b;
                    let v: anyhow::Result<Vec<Vec<Event::<'static>>>> = Result::from_iter(
                        inlines.iter()
                            .map(|b| b.to_events(footnote_number))
                    );
                    // Pandoc AST does not track the type of link.
                    iter::once(Event::Start(Tag::Image { link_type: pulldown_cmark::LinkType::Inline, dest_url: CowStr::Boxed(url[..].into()), title: CowStr::Boxed(title[..].into()), id: "".into() }))
                        .chain(v?.into_iter().flat_map(|v| v))
                        .chain(iter::once(Event::End(TagEnd::Image)))
                        .collect()
                }
                Note(children) => {
                    let mut v2 = vec![Event::FootnoteReference(format!("{footnote_number}").into())];
                    v2.push(Event::Start(Tag::FootnoteDefinition(format!("{footnote_number}").into())));
                    *footnote_number += 1;
                    let v: anyhow::Result<Vec<Vec<Event::<'static>>>> = Result::from_iter(
                        children.iter()
                            .map(|b| b.to_events(footnote_number))
                    );
                    v2.extend(v?.into_iter().flat_map(|v| v));
                    v2.push(Event::End(TagEnd::FootnoteDefinition));
                    v2
                },
                Span(..) => Err(anyhow!("pulldown-cmark does not support span"))?,
            })
        }
    }

    #[derive(Debug, Deserialize)]
    pub struct Target(String, String);

    #[derive(Debug, Deserialize)]
    pub struct Attr(String, Vec<String>, Vec<(String, String)>);

    #[derive(Debug, Deserialize)]
    #[serde(tag = "t", content = "c")]
    pub enum QuoteType {
        SingleQuote,
        DoubleQuote,
    }

    #[derive(Debug, Deserialize)]
    pub struct Caption(Option<ShortCaption>, Vec<Block>);

    #[derive(Debug, Deserialize)]
    pub struct ShortCaption(Vec<Inline>);

    #[derive(Debug, Deserialize)]
    pub struct ColSpec(Alignment, ColWidth);

    #[derive(Debug, Deserialize)]
    pub struct TableHead(Attr, Vec<Row>);

    #[derive(Debug, Deserialize)]
    pub struct Row(Attr, Vec<Cell>);

    #[derive(Debug, Deserialize)]
    pub struct Cell(Attr, Alignment, u64, u64, Vec<Block>);

    #[derive(Debug, Deserialize)]
    pub struct TableBody(Attr, u64, Vec<Row>, Vec<Row>);

    #[derive(Debug, Deserialize)]
    pub struct TableFoot(Attr, Vec<Row>);

    #[derive(Debug, Deserialize)]
    #[serde(tag = "t", content = "c")]
    pub enum Alignment {
        AlignLeft,
        AlignRight,
        AlignCenter,
        AlignDefault,
    }

    #[derive(Debug, Deserialize)]
    #[serde(tag = "t", content = "c")]
    pub enum ColWidth {
        ColWidth(f64),
        ColWidthDefault,
    }

    #[derive(Debug, Deserialize)]
    pub struct ListAttributes(u64, ListNumberStyle, ListNumberDelim);

    #[derive(Debug, Deserialize)]
    #[serde(tag = "t", content = "c")]
    pub enum ListNumberStyle {
        DefaultStyle,
        Example,
        Decimal,
        LowerRoman,
        UpperRoman,
        LowerAlpha,
        UpperAlpha,
    }
    #[derive(Debug, Deserialize)]
    #[serde(tag = "t", content = "c")]
    pub enum ListNumberDelim {
        DefaultDelim,
        Period,
        OneParen,
        TwoParens,
    }
}

pub use pandoc_ast::Pandoc as PandocAst;

pub fn normalize_pandoc(events: Vec<Event<'_>>) -> Vec<Event<'_>> {
    let mut normalized = normalize(events);
    for event in normalized.iter_mut() {
        match event {
            // Pandoc collapses spaces
            Event::Text(p) | Event::InlineMath(p) | Event::DisplayMath(p) if p.contains("  ") || p.contains("\n") => {
                let mut st = String::with_capacity(p.len());
                let mut prev_char_is_space = false;
                for c in p.chars() {
                    if c == ' ' {
                        if prev_char_is_space {
                            continue;
                        }
                        prev_char_is_space = true;
                        st.push(' ');
                    } else if c == '\n' {
                        if prev_char_is_space {
                            continue;
                        }
                        prev_char_is_space = true;
                        st.push('\n');
                    } else {
                        prev_char_is_space = false;
                        st.push(c);
                    }
                }
                *p = st.into();
            }
            Event::Start(Tag::CodeBlock(fence)) => {
                *fence = CodeBlockKind::Fenced("".into());
            }
            _ => {},
        }
    }
    // empty text nodes aren't generated by pandoc
    normalized.retain(|f| {
        if let Event::Text(p) = f {
            !p.is_empty()
        } else {
            true
        }
    });
    normalized
}

/// Send Markdown `text` to `commonmark.js` and return XML.
pub fn commonmark_js(text: &str) -> anyhow::Result<String> {
    const COMMONMARK_MIN_JS: &str =
        include_str!("../../pulldown-cmark/third_party/commonmark.js/commonmark.min.js");

    thread_local! {
        static ENGINE: JSEngine = {
            JSEngine::init().expect("failed to initalize JS engine")
        }
    }

    ENGINE.with(|engine| {
        let rt = Runtime::new(engine.handle());

        let options = RealmOptions::default();
        rooted!(in(rt.cx()) let global = unsafe {
            JS_NewGlobalObject(rt.cx(), &SIMPLE_GLOBAL_CLASS, ptr::null_mut(),
                                OnNewGlobalHookOption::FireOnNewGlobalHook,
                                &*options)
        });
        let realm = unsafe { EnterRealm(rt.cx(), global.get()) };

        // The return value comes back here. If it could be a GC thing, you must add it to the
        // GC's "root set" with the rooted! macro.
        rooted!(in(rt.cx()) let mut rval = UndefinedValue());

        // These should indicate source location for diagnostics.
        let filename: &'static str = "commonmark.min.js";
        let lineno: u32 = 1;
        let res = rt.evaluate_script(global.handle(), COMMONMARK_MIN_JS, filename, lineno, rval.handle_mut());
        assert!(res.is_ok());

        let filename: &'static str = "{inline}";
        let lineno: u32 = 1;
        let script = r#"
            function render_to_xml(markdown) {
                var reader = new commonmark.Parser();
                var xmlwriter = new commonmark.XmlRenderer({ sourcepos: false });
                return xmlwriter.render(reader.parse(markdown));
            }
        "#;
        rooted!(in(rt.cx()) let mut render_to_xml = UndefinedValue());
        let res = rt.evaluate_script(global.handle(), script, filename, lineno, render_to_xml.handle_mut());
        assert!(res.is_ok());

        // rval now contains a reference to the render_to_xml function
        let xml = unsafe {
            rooted!(in(rt.cx()) let mut xml = UndefinedValue());
            rooted!(in(rt.cx()) let mut text_val = UndefinedValue());
            text.to_jsval(rt.cx(), text_val.handle_mut());
            JS_CallFunctionName(
                rt.cx(),
                global.handle(),
                b"render_to_xml\0".as_ptr() as *const i8,
                &HandleValueArray::from_rooted_slice(&[text_val.handle().get()]),
                xml.handle_mut(),
            );
            let xml_string = xml.handle().to_string();
            let utf8 = mozjs::conversions::jsstr_to_string(rt.cx(), xml_string);
            utf8
        };

        unsafe { LeaveRealm(rt.cx(), realm); }

        Ok(xml)
    })
}

/// Parse commonmark.js XML and return Markdown events.
pub fn xml_to_events(xml: &str) -> anyhow::Result<Vec<Event>> {
    let mut block_container_stack = Vec::new();
    let mut heading_stack = Vec::new();

    let mut reader = Reader::from_str(xml);
    let mut events: Vec<Event> = Vec::new();
    loop {
        match reader.read_event()? {
            XmlEvent::Eof => break,
            XmlEvent::Decl(..) | XmlEvent::DocType(..) => continue,
            XmlEvent::Start(tag) => match tag.name().as_ref() {
                b"document" => continue,
                b"paragraph" if block_container_stack.last().map(|(_start, tight)| *tight).unwrap_or(false) => {
                    continue;
                }
                b"paragraph" => events.push(Event::Start(Tag::Paragraph)),
                b"heading" => match tag.try_get_attribute("level")? {
                    Some(level) => {
                        let level = level
                            .unescape_value()?
                            .parse::<usize>()?
                            .try_into()
                            .map_err(|err| anyhow!("Invalid level: {err:?}"))?;
                        heading_stack.push(level);
                        events.push(Event::Start(Tag::Heading {
                            level,
                            id: None,
                            classes: Vec::new(),
                            attrs: Vec::new(),
                        }));
                    }
                    None => anyhow::bail!("Missing level in heading"),
                },
                b"text" => {
                    events.push(Event::Text(
                        unescape(&reader.read_text(tag.to_end().name())?)?
                            .into_owned()
                            .into(),
                    ));
                }
                b"code_block" => {
                    match tag.try_get_attribute("info")? {
                        Some(info) => events.push(Event::Start(Tag::CodeBlock(
                            CodeBlockKind::Fenced(info.unescape_value()?.into_owned().into()),
                        ))),
                        None => events.push(Event::Start(Tag::CodeBlock(CodeBlockKind::Indented))),
                    }
                    events.push(Event::Text(
                        unescape(&reader.read_text(tag.to_end().name())?)?
                            .into_owned()
                            .into(),
                    ));
                    events.push(Event::End(TagEnd::CodeBlock));
                }
                b"list" => {
                    let start = tag.try_get_attribute("start")?;
                    match &start {
                        Some(start) => events.push(Event::Start(Tag::List(Some(
                            start.unescape_value()?.parse()?,
                        )))),
                        None => events.push(Event::Start(Tag::List(None))),
                    };
                    let tight = match tag.try_get_attribute("tight") {
                        Ok(Some(value)) if value.unescape_value()? == "true" => {
                            true
                        }
                        _ => false,
                    };
                    block_container_stack.push((start.is_some(), tight));
                }
                b"item" => events.push(Event::Start(Tag::Item)),
                b"strong" => events.push(Event::Start(Tag::Strong)),
                b"emph" => events.push(Event::Start(Tag::Emphasis)),
                b"code" => events.push(Event::Code(
                    unescape(&reader.read_text(tag.to_end().name())?)?
                        .into_owned()
                        .into(),
                )),
                name @ (b"link" | b"image") => {
                    let dest_url = tag
                        .try_get_attribute("destination")?
                        .ok_or(anyhow!("Missing destination"))?
                        .unescape_value()?
                        .into_owned()
                        .into();
                    let title = match tag.try_get_attribute("title")? {
                        Some(title) => title.unescape_value()?.into_owned().into(),
                        None => "".into(),
                    };
                    let link_type = LinkType::Inline; // commonmark.js does not distinguish.
                    let id = "".into(); // commonmark.js does not record this.
                    events.push(Event::Start(if name == b"link" {
                        Tag::Link {
                            link_type,
                            dest_url,
                            title,
                            id,
                        }
                    } else {
                        Tag::Image {
                            link_type,
                            dest_url,
                            title,
                            id,
                        }
                    }));
                }
                b"block_quote" => {
                    block_container_stack.push((true, false));
                    events.push(Event::Start(Tag::BlockQuote))
                },
                b"html_block" => {
                    events.push(Event::Start(Tag::HtmlBlock));
                    events.push(Event::Html(
                        unescape(&reader.read_text(tag.to_end().name())?)?
                            .into_owned()
                            .into(),
                    ));
                    events.push(Event::End(TagEnd::HtmlBlock));
                },
                b"html_inline" => events.push(Event::InlineHtml(
                    unescape(&reader.read_text(tag.to_end().name())?)?
                        .into_owned()
                        .into(),
                )),
                name => anyhow::bail!("start tag: {}", String::from_utf8_lossy(name)),
            },
            XmlEvent::End(tag) => match tag.name().as_ref() {
                b"document" => continue,
                b"paragraph" if block_container_stack.last().map(|(_numbered, tight)| *tight).unwrap_or(false) => {
                    continue;
                }
                b"paragraph" => events.push(Event::End(TagEnd::Paragraph)),
                b"heading" => events.push(Event::End(TagEnd::Heading(
                    heading_stack.pop().ok_or(anyhow!("Heading stack empty"))?,
                ))),
                b"list" => events.push(Event::End(TagEnd::List(
                    block_container_stack.pop().ok_or(anyhow!("List stack empty"))?.0,
                ))),
                b"item" => events.push(Event::End(TagEnd::Item)),
                b"emph" => events.push(Event::End(TagEnd::Emphasis)),
                b"strong" => events.push(Event::End(TagEnd::Strong)),
                b"link" => events.push(Event::End(TagEnd::Link)),
                b"image" => events.push(Event::End(TagEnd::Image)),
                b"block_quote" => {
                    block_container_stack.pop().ok_or(anyhow!("List stack empty"))?;
                    events.push(Event::End(TagEnd::BlockQuote))
                },
                name => anyhow::bail!("end tag: {}", String::from_utf8_lossy(name)),
            },
            XmlEvent::Text(_) => continue,
            XmlEvent::Empty(tag) => match tag.name().as_ref() {
                b"thematic_break" => events.push(Event::Rule),
                b"softbreak" => events.push(Event::SoftBreak),
                b"linebreak" => events.push(Event::HardBreak),
                name => anyhow::bail!("empty tag: {}", String::from_utf8_lossy(name)),
            },
            event => anyhow::bail!("event {event:?}"),
        }
    }

    Ok(events)
}

/// Normalize Markdown events
///
/// - Joins adjacent `Event::Text` and `Event::Html` events.
///
/// - Ensures every `Tag::Item` has a `Tag::Paragraph` as its first
///   child (commonmark.js tracks items loseness via an attribute).
///
/// - Adds a final newline to non-empty `Tag::CodeBlock` tags.
///
/// - Resets the link type to `LinkType::Inline`.
///
/// - Resets all code blocks to `CodeBlockKind::Fenced`.
pub fn normalize(events: Vec<Event<'_>>) -> Vec<Event<'_>> {
    let mut footnotes: HashMap<String, Vec<Event<'_>>> = HashMap::new();
    let mut in_footnote: Vec<usize> = Vec::new();
    let mut normalized = Vec::with_capacity(events.len());
    for event in events.into_iter() {
        match (normalized.last_mut(), &event) {
            // Join adjacent text and HTML events.
            (Some(Event::Text(prev)), Event::Text(next)) => *prev = format!("{prev}{next}").into(),
            (Some(Event::Html(prev)), Event::Html(next)) => *prev = format!("{prev}{next}").into(),

            // commonmark.js always adds a final newline to code blocks.
            (Some(Event::Text(prev)), Event::End(TagEnd::CodeBlock)) => {
                *prev = prev.trim_end().to_string().into();
                normalized.push(event);
            }

            (Some(Event::SoftBreak), Event::SoftBreak) => {}
            (Some(Event::HardBreak), Event::SoftBreak) | (Some(Event::SoftBreak), Event::HardBreak) => {
                normalized.pop();
                normalized.push(Event::HardBreak);
            }
            (Some(Event::HardBreak | Event::SoftBreak), Event::Text(text)) if text.bytes().all(|c| c.is_ascii_whitespace() || c == b'\t') => {}
            (Some(Event::HardBreak | Event::SoftBreak), Event::Text(text)) => {
                normalized.push(Event::Text(text.trim_start_matches(|c: char| c.is_ascii_whitespace() || c == '\t').to_string().into()))
            }
            (Some(Event::Text(text)), Event::HardBreak | Event::SoftBreak) => {
                *text = text.trim_end_matches(|c: char| c.is_ascii_whitespace()).to_string().into();
                normalized.push(event);
            }
            (Some(Event::Text(prev)), Event::Html(next)) if prev.bytes().all(|c| c.is_ascii_whitespace() || c == b'\t') => {
                normalized.pop();
                normalized.push(Event::Html(next.trim_start_matches(|c: char| c.is_ascii_whitespace() || c == '\t').to_string().into()));
            },
            (_, Event::Html(text)) => {
                normalized.push(Event::Html(text.trim_start_matches(|c: char| c.is_ascii_whitespace() || c == '\t').to_string().into()));
            }

            (Some(Event::End(TagEnd::Emphasis)), Event::Start(Tag::Emphasis)) => {
                normalized.pop();
            }
            (Some(Event::End(TagEnd::Strong)), Event::Start(Tag::Strong)) => {
                normalized.pop();
            }

            (_, Event::Start(Tag::FootnoteDefinition(..))) => {
                in_footnote.push(normalized.len());
                normalized.push(event);
            }

            (_, Event::End(TagEnd::FootnoteDefinition)) => {
                let j = in_footnote.pop().unwrap();
                normalized.push(event);
                let footnote = normalized.split_off(j);
                let name = match &footnote[0] {
                    Event::Start(Tag::FootnoteDefinition(name)) => name.clone(),
                    _ => panic!("this should not be possible {footnote:?}", footnote = footnote),
                };
                footnotes.entry(name.to_string()).or_insert(footnote);
            }

            // Other events are passed through.
            (_, _) => normalized.push(event),
        }
    }

    let mut current_footnote_number = 0;
    normalized
        .into_iter()
        .flat_map(|event| match event {
            // other markdown parsers usually don't expose raw
            // footnotes definitions
            Event::FootnoteReference(name) => {
                current_footnote_number += 1;
                let mut footnotes = footnotes.get(&name[..]).cloned().unwrap_or_default();
                if !footnotes.is_empty() {
                    footnotes[0] = Event::Start(Tag::FootnoteDefinition(format!("{current_footnote_number}").into()));
                }
                let mut normalized = Vec::with_capacity(1 + footnotes.len());
                normalized.push(Event::FootnoteReference(format!("{current_footnote_number}").into()));
                normalized.extend(footnotes);
                normalized
            }
            // commonmark.js does not record the link type.
            Event::Start(Tag::Link {
                link_type: LinkType::Email,
                dest_url,
                title,
                ..
            }) => vec![Event::Start(Tag::Link {
                link_type: LinkType::Inline,
                dest_url: urldecode(&format!("mailto:{dest_url}")).into(),
                title: title.clone(),
                id: "".into(), // commonmark.js does not record this
            })],
            Event::Start(Tag::Link {
                dest_url,
                title,
                ..
            }) => vec![Event::Start(Tag::Link {
                link_type: LinkType::Inline,
                dest_url: urldecode(&dest_url).into(),
                title: title.clone(),
                id: "".into(), // commonmark.js does not record this
            })],
            // commonmark.js does not record the link type.
            Event::Start(Tag::Image {
                dest_url,
                title,
                ..
            }) => vec![Event::Start(Tag::Image {
                link_type: LinkType::Inline,
                dest_url: urldecode(&dest_url).into(),
                title: title.clone(),
                id: "".into(),
            })],
            // commonmark.js does not distinguish between fenced code
            // blocks with a "" info string and indented code blocks.
            Event::Start(Tag::CodeBlock(CodeBlockKind::Indented)) => vec![Event::Start(
                Tag::CodeBlock(CodeBlockKind::Fenced("".into())),
            )],

            // pulldown-cmark can generate empty text and HTML events.
            Event::Text(text) if text.is_empty() => vec![],
            Event::Html(html) if html.is_empty() => vec![],

            // pulldown-cmark includes trailing newlines in HTML.
            Event::Html(html) => vec![Event::Html(html.trim_end_matches('\n').to_string().into())],

            event => vec![event],
        })
        .collect()
}

/// Print Markdown events with indentation.
///
/// The `text` label indicates the source of the events.
pub fn print_events(text: &str, events: &[Event]) {
    eprintln!("{text:?} -> [");
    let mut width: usize = 0;
    for event in events {
        if let Event::End(_) = event {
            width = width.saturating_sub(2);
        }
        eprintln!("  {:width$}{event:?}", "");
        if let Event::Start(_) = event {
            width = width.saturating_add(2);
        }
    }
    eprintln!("]");
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use pulldown_cmark::{CodeBlockKind, Event, Tag, TagEnd};

    #[test]
    fn test_normalize_text() {
        assert_eq!(
            normalize(vec![
                Event::Text("foo".into()),
                Event::Text("bar".into()),
                Event::Text("baz".into())
            ]),
            vec![Event::Text("foobarbaz".into())]
        );
    }

    #[test]
    fn test_normalize_empty_text() {
        assert_eq!(normalize(vec![Event::Text("".into())]), vec![]);
    }

    #[test]
    fn test_normalize_html() {
        assert_eq!(
            normalize(vec![
                Event::Html("<foo>".into()),
                Event::Html("<bar>".into()),
                Event::Html("<baz>".into())
            ]),
            vec![Event::Html("<foo><bar><baz>".into())]
        );
    }

    #[test]
    fn test_normalize_empty_html() {
        assert_eq!(normalize(vec![Event::Html("".into())]), vec![]);
    }

    #[test]
    fn test_normalize_non_empty_list() {
        assert_eq!(
            normalize(vec![
                Event::Start(Tag::List(None)),
                Event::Start(Tag::Item),
                Event::Text("foo".into()),
                Event::End(TagEnd::Item),
                Event::End(TagEnd::List(false)),
            ]),
            vec![
                Event::Start(Tag::List(None)),
                Event::Start(Tag::Item),
                Event::Start(Tag::Paragraph),
                Event::Text("foo".into()),
                Event::End(TagEnd::Paragraph),
                Event::End(TagEnd::Item),
                Event::End(TagEnd::List(false)),
            ]
        );
    }

    #[test]
    fn test_normalize_empty_list() {
        assert_eq!(
            normalize(vec![
                Event::Start(Tag::List(None)),
                Event::Start(Tag::Item),
                Event::End(TagEnd::Item),
                Event::End(TagEnd::List(false)),
            ]),
            vec![
                Event::Start(Tag::List(None)),
                Event::Start(Tag::Item),
                Event::End(TagEnd::Item),
                Event::End(TagEnd::List(false)),
            ]
        );
    }

    #[test]
    fn test_normalize_empty_code_block() {
        assert_eq!(
            normalize(vec![
                Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced("rust".into()))),
                Event::Text("".into()),
                Event::End(TagEnd::CodeBlock)
            ]),
            vec![
                Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced("rust".into()))),
                Event::End(TagEnd::CodeBlock)
            ]
        );
    }

    #[test]
    fn test_normalize_non_empty_code_block() {
        assert_eq!(
            normalize(vec![
                Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced("rust".into()))),
                Event::Text("fn main() {}".into()),
                Event::End(TagEnd::CodeBlock)
            ]),
            vec![
                Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced("rust".into()))),
                Event::Text("fn main() {}\n".into()),
                Event::End(TagEnd::CodeBlock)
            ]
        );
    }

    #[test]
    fn test_normalize_footnotes() {
        assert_eq!(
            normalize(vec![
                Event::FootnoteReference("foo".into()),
                Event::FootnoteReference("foo".into()),
                Event::FootnoteReference("foo".into()),
                Event::Start(Tag::FootnoteDefinition("foo".into())),
                Event::Start(Tag::Paragraph),
                Event::Text("me".into()),
                Event::End(TagEnd::Paragraph),
                Event::End(TagEnd::FootnoteDefinition)
            ]),
            vec![
                Event::FootnoteReference("1".into()),
                Event::Start(Tag::FootnoteDefinition("1".into())),
                Event::Start(Tag::Paragraph),
                Event::Text("me".into()),
                Event::End(TagEnd::Paragraph),
                Event::End(TagEnd::FootnoteDefinition),
                Event::FootnoteReference("2".into()),
                Event::Start(Tag::FootnoteDefinition("2".into())),
                Event::Start(Tag::Paragraph),
                Event::Text("me".into()),
                Event::End(TagEnd::Paragraph),
                Event::End(TagEnd::FootnoteDefinition),
                Event::FootnoteReference("3".into()),
                Event::Start(Tag::FootnoteDefinition("3".into())),
                Event::Start(Tag::Paragraph),
                Event::Text("me".into()),
                Event::End(TagEnd::Paragraph),
                Event::End(TagEnd::FootnoteDefinition),
            ]
        );
    }
}
