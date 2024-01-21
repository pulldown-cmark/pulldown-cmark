use pulldown_cmark::{Event, Options, Parser, Tag};

fn main() {
    let markdown_input = r##"
# My Heading

My paragraph.
[reference to my heading](#my-heading)

* a
* b
* c

1. d
2. e
3. f

Task lists
- [ ] to do
- [x] done

A block quote
> my block quote

```
my code block
```

*emphasis*
**strong**
~~strikethrough~~
[My Link](http://example.com)
![My Image](http://example.com/image.jpg)

| a | b |
| - | - |
| c | d |

hello[^1]
[^1]: my footnote
"##;
    println!("Parsing the following markdown string:\n{}", markdown_input);

    // Set up the parser. We can treat is as any other iterator.
    // For each event, we print its details, such as the tag or string.
    // This filter simply returns the same event without any changes;
    // you can compare the `event-filter` example which alters the output.
    let parser = Parser::new_ext(markdown_input, Options::all()).map(|event| {
        match &event {
            Event::Start(tag) => match tag {
                Tag::HtmlBlock => println!("HtmlBlock"),
                Tag::Heading {
                    level,
                    id,
                    classes,
                    attrs,
                } => println!(
                    "Heading heading_level: {} fragment identifier: {:?} classes: {:?} attrs: {:?}",
                    level, id, classes, attrs
                ),
                Tag::Paragraph => println!("Paragraph"),
                Tag::List(ordered_list_first_item_number) => println!(
                    "List: if ordered, first item number: {:?}",
                    ordered_list_first_item_number
                ),
                Tag::Item(marker) => println!("Item: if GFM task item, is_checked: {:?}", marker),
                Tag::Emphasis => println!("Emphasis (this is a span tag)"),
                Tag::Strong => println!("Strong (this is a span tag)"),
                Tag::Strikethrough => println!("Strikethrough (this is a span tag)"),
                Tag::BlockQuote => println!("BlockQuote"),
                Tag::CodeBlock(code_block_kind) => {
                    println!("CodeBlock code_block_kind: {:?}", code_block_kind)
                }
                Tag::Link {
                    link_type,
                    dest_url,
                    title,
                    id,
                } => println!(
                    "Link link_type: {:?} url: {} title: {} id: {}",
                    link_type, dest_url, title, id
                ),
                Tag::Image {
                    link_type,
                    dest_url,
                    title,
                    id,
                } => println!(
                    "Image link_type: {:?} url: {} title: {} id: {}",
                    link_type, dest_url, title, id
                ),
                Tag::Table(column_text_alignment_list) => println!(
                    "Table column_text_alignment_list: {:?}",
                    column_text_alignment_list
                ),
                Tag::TableHead => println!("TableHead (contains TableRow tags"),
                Tag::TableRow => println!("TableRow (contains TableCell tags)"),
                Tag::TableCell => println!("TableCell (contains inline tags)"),
                Tag::FootnoteDefinition(label) => println!("FootnoteDefinition label: {}", label),
                Tag::MetadataBlock(kind) => println!("MetadataBlock: {:?}", kind),
            },
            _ => (),
        };
        event
    });

    let mut html_output = String::new();
    pulldown_cmark::html::push_html(&mut html_output, parser);
    println!("\nHTML output:\n{}\n", &html_output);
}
