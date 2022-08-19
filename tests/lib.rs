use html5ever::serialize::{serialize, SerializeOpts};
use html5ever::{driver as html, local_name, namespace_url, ns, QualName};
use markup5ever_rcdom::{Handle, NodeData, RcDom, SerializableHandle};
use pulldown_cmark::{Options, Parser};

use regex::Regex;
use std::collections::HashSet;
use std::mem;
use std::rc::{Rc, Weak};
use tendril::stream::TendrilSink;

mod suite;

#[inline(never)]
pub fn test_markdown_html(input: &str, output: &str, smart_punct: bool) {
    let mut s = String::new();

    let mut opts = Options::empty();
    opts.insert(Options::ENABLE_TABLES);
    opts.insert(Options::ENABLE_FOOTNOTES);
    opts.insert(Options::ENABLE_STRIKETHROUGH);
    opts.insert(Options::ENABLE_TASKLISTS);
    if smart_punct {
        opts.insert(Options::ENABLE_SMART_PUNCTUATION);
    }
    opts.insert(Options::ENABLE_HEADING_ATTRIBUTES);

    let p = Parser::new_ext(input, opts);
    pulldown_cmark::html::push_html(&mut s, p);

    assert_eq!(normalize_html(output), normalize_html(&s));
}

lazy_static::lazy_static! {
    static ref WHITESPACE_RE: Regex = Regex::new(r"\s+").unwrap();
    static ref LEADING_WHITESPACE_RE: Regex = Regex::new(r"\A\s+").unwrap();
    static ref TRAILING_WHITESPACE_RE: Regex = Regex::new(r"\s+\z").unwrap();
    static ref BLOCK_TAGS: HashSet<&'static str> = [
        "article",
        "header",
        "aside",
        "hgroup",
        "blockquote",
        "hr",
        "iframe",
        "body",
        "li",
        "map",
        "button",
        "object",
        "canvas",
        "ol",
        "caption",
        "output",
        "col",
        "p",
        "colgroup",
        "pre",
        "dd",
        "progress",
        "div",
        "section",
        "dl",
        "table",
        "td",
        "dt",
        "tbody",
        "embed",
        "textarea",
        "fieldset",
        "tfoot",
        "figcaption",
        "th",
        "figure",
        "thead",
        "footer",
        "tr",
        "form",
        "ul",
        "h1",
        "h2",
        "h3",
        "h4",
        "h5",
        "h6",
        "video",
        "script",
        "style"
    ]
    .iter()
    .cloned()
    .collect();
    static ref WHITESPACE_SENSITIVE_TAGS: HashSet<&'static str> =
        ["pre", "code", "h1", "h2", "h3", "h4", "h5", "h6"]
            .iter()
            .cloned()
            .collect();
    static ref TABLE_TAGS: HashSet<&'static str> = ["table", "thead", "tbody", "tr", "td"]
        .iter()
        .cloned()
        .collect();
}

fn make_html_parser() -> html::Parser<RcDom> {
    html::parse_fragment(
        RcDom::default(),
        html::ParseOpts::default(),
        QualName::new(None, ns!(html), local_name!("div")),
        vec![],
    )
}

fn normalize_html(s: &str) -> String {
    let parser = make_html_parser();
    let dom = parser.one(s);
    let body: SerializableHandle = normalize_dom(&dom).into();
    let opts = SerializeOpts::default();
    let mut ret_val = Vec::new();
    serialize(&mut ret_val, &body, opts)
        .expect("Writing to a string shouldn't fail (expect on OOM)");
    String::from_utf8(ret_val).expect("html5ever should always produce UTF8")
}

fn normalize_dom(dom: &RcDom) -> Handle {
    let body = {
        let children = dom.document.children.borrow();
        children[0].clone()
    };
    let mut current_level = Vec::new();
    let mut next_level = Vec::new();
    current_level.extend(body.children.borrow().iter().cloned().rev());
    loop {
        while let Some(mut node) = current_level.pop() {
            let parent = node.parent.replace(None);
            node.parent.replace(parent.clone());
            let parent = parent
                .expect("a node in the DOM will have a parent, except the root, which is not processed")
                .upgrade().expect("a node's parent will be pointed to by its parent (or the root pointer), and will not be dropped");
            let retain = normalize_node(&parent, &mut node);
            if !retain {
                let mut siblings = parent.children.borrow_mut();
                siblings.retain(|s| !Rc::ptr_eq(&node, s));
            } else {
                next_level.extend(node.children.borrow().iter().cloned().rev());
            }
        }
        if next_level.is_empty() {
            break;
        };
        mem::swap(&mut next_level, &mut current_level);
    }
    body
}

// Returns false if node is an empty text node or an empty tbody.
// Returns true otherwise.
fn normalize_node(parent: &Handle, node: &mut Handle) -> bool {
    match node.data {
        NodeData::Comment { .. }
        | NodeData::Doctype { .. }
        | NodeData::Document
        | NodeData::ProcessingInstruction { .. } => true,
        NodeData::Text { ref contents, .. } => {
            let mut contents = contents.borrow_mut();
            let is_pre = {
                let mut parent = parent.clone();
                loop {
                    let is_pre = if let NodeData::Element { ref name, .. } = parent.data {
                        WHITESPACE_SENSITIVE_TAGS.contains(&&*name.local.to_ascii_lowercase())
                    } else {
                        false
                    };
                    if is_pre {
                        break true;
                    };
                    let parent_ = parent.parent.replace(None);
                    parent.parent.replace(parent_.clone());
                    let parent_ = parent_.as_ref().and_then(Weak::upgrade);
                    if let Some(parent_) = parent_ {
                        parent = parent_
                    } else {
                        break false;
                    };
                }
            };
            if !is_pre {
                let (is_first_in_block, is_last_in_block) = {
                    let mut is_first_in_block = true;
                    let mut is_last_in_block = true;
                    let mut parent = parent.clone();
                    let mut node = node.clone();
                    loop {
                        let reached_block = if let NodeData::Element { ref name, .. } = parent.data
                        {
                            BLOCK_TAGS.contains(&&*name.local.to_ascii_lowercase())
                        } else {
                            false
                        };
                        let (is_first, is_last) = {
                            let siblings = parent.children.borrow();
                            let n = &node;
                            (
                                siblings.get(0).map(|s| Rc::ptr_eq(s, n)).unwrap_or(false),
                                siblings.len() > 0
                                    && siblings
                                        .get(siblings.len() - 1)
                                        .map(|s| Rc::ptr_eq(s, n))
                                        .unwrap_or(false),
                            )
                        };
                        is_first_in_block = is_first_in_block && is_first;
                        is_last_in_block = is_last_in_block && is_last;
                        if (is_first_in_block || is_last_in_block) && !reached_block {
                            node = parent.clone();
                            let parent_ = parent.parent.replace(None);
                            parent.parent.replace(parent_.clone());
                            let parent_ = parent_.as_ref().and_then(Weak::upgrade);
                            if let Some(parent_) = parent_ {
                                parent = parent_;
                            } else {
                                break (is_first_in_block, is_last_in_block);
                            }
                        } else {
                            break (is_first_in_block, is_last_in_block);
                        }
                    }
                };
                let is_preceeded_by_ws = {
                    let mut parent = parent.clone();
                    let mut node = node.clone();
                    'ascent: loop {
                        let is_first = {
                            let siblings = parent.children.borrow();
                            let n = &node;
                            siblings.get(0).map(|s| Rc::ptr_eq(s, n)).unwrap_or(false)
                        };
                        if is_first {
                            node = parent.clone();
                            let parent_ = parent.parent.replace(None);
                            parent.parent.replace(parent_.clone());
                            let parent_ = parent_.as_ref().and_then(Weak::upgrade);
                            if let Some(parent_) = parent_ {
                                parent = parent_;
                            } else {
                                break 'ascent false;
                            }
                        } else {
                            let siblings = parent.children.borrow();
                            let n = &node;
                            let mut pos = !0;
                            'search: for (i, s) in siblings.iter().enumerate() {
                                if Rc::ptr_eq(s, n) {
                                    pos = i;
                                    break 'search;
                                }
                            }
                            assert!(
                                pos != !0,
                                "The list of node's parent's children shall contain node"
                            );
                            assert!(
                                pos != 0,
                                "If node is not first, then node's position shall not be zero"
                            );
                            let mut preceding = siblings[pos - 1].clone();
                            'descent: loop {
                                if let NodeData::Text { .. } = preceding.data {
                                    break 'descent;
                                }
                                preceding = {
                                    let ch = preceding.children.borrow();
                                    if ch.len() == 0 {
                                        break 'descent;
                                    }
                                    if let Some(preceeding_) = ch.get(ch.len() - 1) {
                                        preceeding_.clone()
                                    } else {
                                        break 'descent;
                                    }
                                };
                            }
                            if let NodeData::Text { ref contents, .. } = preceding.data {
                                break 'ascent TRAILING_WHITESPACE_RE.is_match(&*contents.borrow());
                            } else {
                                break 'ascent false;
                            }
                        }
                    }
                };

                let is_in_table = if let NodeData::Element { ref name, .. } = parent.data {
                    TABLE_TAGS.contains(&&*name.local.to_ascii_lowercase())
                } else {
                    false
                };
                let whitespace_replacement = if is_in_table { "" } else { " " };
                *contents = WHITESPACE_RE
                    .replace_all(&*contents, whitespace_replacement)
                    .as_ref()
                    .into();

                if is_first_in_block || is_preceeded_by_ws {
                    *contents = LEADING_WHITESPACE_RE
                        .replace_all(&*contents, "")
                        .as_ref()
                        .into();
                }
                if is_last_in_block {
                    *contents = TRAILING_WHITESPACE_RE
                        .replace_all(&*contents, "")
                        .as_ref()
                        .into();
                }
                // TODO: collapse whitespace when adjacent to whitespace.
                // For example, the whitespace in the span should be collapsed in all of these cases:
                //
                //     " <span> q </span> "
                //     "<b>q </b><span> q</span>"
                //     "<b>q <i></i></b><span> q</span>"
                //     "<b>q <i></i></b><span> q</span>"
                //     "q <b></b><span> q</span>"
            }
            &**contents != ""
        }
        NodeData::Element {
            ref attrs,
            ref name,
            ..
        } => {
            let mut attrs = attrs.borrow_mut();
            for a in attrs.iter_mut() {
                a.name.local = a.name.local.to_ascii_lowercase().into();
            }
            attrs.sort_by(|a: &html5ever::Attribute, b: &html5ever::Attribute| {
                (&*a.name.local).cmp(&*b.name.local)
            });
            let ascii_name = &*name.local.to_ascii_lowercase();
            // drop empty tbody's
            ascii_name != "tbody"
                || node.children.borrow().len() > 1
                || node
                    .children
                    .borrow()
                    .iter()
                    .next()
                    .map(|only_child| match only_child.data {
                        NodeData::Text { ref contents, .. } => {
                            !contents.borrow().chars().all(|c| c.is_whitespace())
                        }
                        _ => true,
                    })
                    .unwrap_or(false)
        }
    }
}

#[test]
fn strip_div_newline() {
    assert_eq!("<div></div>", normalize_html("<div>\n</div>"));
}

#[test]
fn strip_end_newline() {
    assert_eq!("test", normalize_html("test\n"));
}

#[test]
fn strip_double_space() {
    assert_eq!("test mess", normalize_html("test  mess"));
}

#[test]
fn strip_inline_internal_text() {
    assert_eq!(
        "<u>a </u>b <u>c</u>",
        normalize_html("<u> a </u> b <u> c </u>")
    )
}

#[test]
fn strip_inline_block_internal_text() {
    assert_eq!(
        "<u>a </u>b <u>c</u>",
        normalize_html(" <u> a </u> b <u> c </u> ")
    )
}

#[test]
fn leaves_necessary_whitespace_alone() {
    assert_eq!("<u>a</u> b <u>c</u>", normalize_html("<u>a</u> b <u>c</u>"))
}

#[test]
fn leaves_necessary_whitespace_alone_weird() {
    assert_eq!(
        "<u>a </u>b <u>c</u>",
        normalize_html(" <u>a </u>b <u>c</u>")
    )
}

#[test]
fn leaves_necessary_whitespace_all_nested() {
    assert_eq!(
        "<u></u><u></u><u></u><u></u>",
        normalize_html("<u> </u><u> </u><u> </u><u> </u>")
    )
}

#[test]
fn drops_empty_tbody() {
    assert_eq!(
        "<table><thead><tr><td>hi</td></tr></thead></table>",
        normalize_html("<table><thead><tr><td>hi</td></tr></thead><tbody>  </tbody></table>")
    )
}

#[test]
fn leaves_nonempty_tbody() {
    let input = "<table><thead><tr><td>hi</td></tr></thead><tbody><tr></tr></tbody></table>";
    assert_eq!(input, normalize_html(input))
}
