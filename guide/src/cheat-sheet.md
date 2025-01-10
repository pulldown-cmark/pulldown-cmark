# Cheat sheet

## Base syntax

<!-- can't put paragraph breaks in markdown tables, so we gotta use html -->

<table>

<thead>
<th>
<th>Markdown
<th>Result

<tbody>

<tr><th>

[Emphasis](third_party/spec.md#emphasis-and-strong-emphasis)

<td>

<dl><dt>Weak</dt><dd>

    *text* _text_

</dd><dt>Strong</dt><dd>

    **text** __text__

<td>

<dl><dt>Weak</dt><dd>

*text* _text_

</dd><dt>String</dt><dd>

**text** __text__

<tr><th>

[Code span](third_party/spec.md#code-spans)

<td>

    `text`

<td>

`text`

<tr><th>

[Paragraph](third_party/spec.md#paragraphs)

<td>

    first paragraph

    second paragraph

<td>

first paragraph

second paragraph

<tr><th>

[Autolink](third_party/spec.md#autolinks)

<td>

    <https://example.com/destination>

</dd></dl>

<td>

<https://example.com/destination>

<tr><th>

[Link](third_party/spec.md#links)

<td>

<dl><dt>Inline</dt><dd>

    [label](https://example.com/destination)

</dd><dt>Reference</dt><dd>

    [label][ref]

    [ref]: https://example.com/destination

</dd></dl>

<td>

[label](https://example.com/destination)

<tr><th>

[Image](third_party/spec.md#images)

<td>

<dl><dt>Inline</dt><dd>

    ![rust logo](rust-logo-151179464ae7ed46.svg)

</dd><dt>Reference</dt><dd>

    ![rust logo][ref]

    [ref]: rust-logo-151179464ae7ed46.svg

</dd></dl>

<td>

![rust logo](rust-logo-151179464ae7ed46.svg)

<tr><th>

[Thematic break](third_party/spec.md#thematic-breaks)

<td>

    ***

    ---

    ___

<td>

***

<tr><th>Heading

<td>

<dl><dt>

[ATX style (h1-h6)](third_party/spec.md#atx-headings)

</dt><dd>

    # H1
    ## H2
    ...
    ###### H6

</dd><dt>

[Setext style (h1/h2 only)](third_party/spec.md#setext-headings)

</dt><dd>

    H1
    ==

    H2
    --

</dd></dl>

<td>

H1
==

H2
--

<tr><th>

[Fenced code](third_party/spec.md#fenced-code-blocks)

<td>

    ```rust
    fn main() {}
    ```

    ~~~rust
    fn main() {}
    ~~~

<td>

~~~rust
fn main() {}
~~~

<tr><th>

[Indented code](third_party/spec.md#indented-code-blocks)

<td>

    ••••fn main() {}

<td>

    fn main() {}

<tr><th>

[Block quote](third_party/spec.md#block-quotes)

<td>

    > Don't believe everything you
    > read on the Internet.
    >
    > — Abraham Lincoln

<td>

> Don't believe everything you
> read on the Internet.
>
> — Abraham Lincoln

<tr><th>

[Numbered list](third_party/spec.md#list-items)

<td>

    1. first
    2. second

<td>

1. first
2. second

<tr><th>

[Bulleted list](third_party/spec.md#list-items)

<td>

    - first
    - second
    * first
    * second
    + first
    + second

<td>

- first
- second
* first
* second
+ first
+ second

<tr><th>

[Hard line break](third_party/spec.md#hard-line-breaks)

<td>

    a\
    b••
    c

<td>

a\
b  
c

</table>

## GitHub-flavored Markdown

<table>

<thead>
<th>
<th>Markdown
<th>Result

<tbody>

<tr><th>

[Table](specs/table.md)

<td>

    | header | row |
    | ------ | --- |
    | body   | row |

<td>

| header | row |
| ------ | --- |
| body   | row |

<tr><th>

[Math](specs/math.md)

<td>

<dl><dt>Inline</dt><dd>

    $2+2=4$

</dd><dt>Display</dt><dd>

    $$x=\frac{-b\pm\sqrt{b^2-4ac} }{2a}$$

</dd></dl>

<td>

<dl><dt>Inline</dt><dd>

<!-- we're running too old an mdbook version,
     and are using the MathJax plugin right now -->

\\(2+2=4\\)

</dd><dt>Display</dt><dd>

\\[x=\frac{ -b\pm\sqrt{b^2-4ac} }{2a}\\]

</dd></dl>

<tr><th>

[Blockquote tags](specs/blockquotes_tags.md)

<td>

    > [!WARNING]
    >
    > Huh?

<td>

<div class="warning">

Huh?

</div>

<tr><th>

[Footnote](specs/footnotes.md)

<td>

    footnote [^reference]

    [^reference]: definition

<td>

<!-- I don't use a real footnote here because I want the definition inline in the cell -->

footnote <sup>[<a href="#preview-footnote-reference">1</a>]</sup>

<small>[1]: definition</small>

<tr><th>

[Strikethrough](specs/strikethrough.md)

<td>

    ~removed text~

    ~~also removed text~~

<td>

~removed text~

~~also removed text~~

<tr><th>

[Tasklist](third_party/gfm_tasklist.md)

<td>

    - [ ] incomplete
    - [x] complete

<td>

- [ ] incomplete
- [x] complete

</table>

## Other Extensions

<table>

<thead>
<th>
<th>Markdown
<th>Result

<tbody>

<tr><th>

[Metadata block](specs/metadata_blocks.md)

<td>

    ---
    title: Cheat sheet
    description: Sample syntax of all pulldown-cmark markdown extensions
    ---

<td>

<!-- metadata blocks are required at the beginning of the document, so can't be used here -->

|    title    |                       description                       |
| ----------- | ------------------------------------------------------- |
| Cheat sheet | Sample syntax of all pulldown-cmark markdown extensions |

<tr><th>

[Heading attributes](specs/heading_attrs.md)

<td>

    # Custom heading {.red #custom-heading}

<td>

<h1 style="color:red" id="custom-heading">Custom heading</h1>

<tr><th>

[WikiLinks](specs/wikilinks.md)

<td>

    [[https://example.com/destination|label]]

<td>

<a href="https://example.com/destination">label</a>

</table>
