// This file is auto-generated by the build script
// Please, do not modify it manually

use super::test_markdown_html;

#[test]
fn math_test_1() {
    let original = r##"This sentence uses `$` delimiters to show math inline: $\sqrt{3x-1}+(1+x)^2$
$\sum_{k=1}^n a_k b_k$: Mathematical expression at head of line

`\` may follow just after the first `$`: $\{1, 2, 3\}$
"##;
    let expected = r##"<p>This sentence uses <code>$</code> delimiters to show math inline: <span class="math inline">\sqrt{3x-1}+(1+x)^2</span>
<span class="math inline">\sum_{k=1}^n a_k b_k</span>: Mathematical expression at head of line</p>
<p><code>\</code> may follow just after the first <code>$</code>: <span class="math inline">\{1, 2, 3\}</span>
"##;

    test_markdown_html(original, expected, false, false, false);
}

#[test]
fn math_test_2() {
    let original = r##"**The Cauchy-Schwarz Inequality**

$$\left( \sum_{k=1}^n a_k b_k \right)^2 \leq \left( \sum_{k=1}^n a_k^2 \right) \left( \sum_{k=1}^n b_k^2 \right)$$
"##;
    let expected = r##"<p><strong>The Cauchy-Schwarz Inequality</strong></p>
<p><span class="math display">\left( \sum_{k=1}^n a_k b_k \right)^2 \leq \left( \sum_{k=1}^n a_k^2 \right) \left( \sum_{k=1}^n b_k^2 \right)</span></p>
"##;

    test_markdown_html(original, expected, false, false, false);
}

#[test]
fn math_test_3() {
    let original = r##"Oops empty $$ expression.

$$$$
"##;
    let expected = r##"<p>Oops empty $$ expression.</p>
<p><span class="math display"></span></p>
"##;

    test_markdown_html(original, expected, false, false, false);
}

#[test]
fn math_test_4() {
    let original = r##"$a<b>c</b>$

$${a*b*c} _c_ d$$

$not `code`$

$![not an](/image)$

$<https://not.a.link/>$

$&alpha;$
"##;
    let expected = r##"<p><span class="math inline">a&lt;b&gt;c&lt;/b&gt;</span></p>
<p><span class="math display">{a*b*c} _c_ d</span></p>
<p><span class="math inline">not `code`</span></p>
<p><span class="math inline">![not an](/image)</span></p>
<p><span class="math inline">&lt;https://not.a.link/&gt;</span></p>
<p><span class="math inline">&amp;alpha;</span></p>
"##;

    test_markdown_html(original, expected, false, false, false);
}

#[test]
fn math_test_5() {
    let original = r##"Hello $world.

Dollar at end of line$
"##;
    let expected = r##"<p>Hello $world.</p>
<p>Dollar at end of line$</p>
"##;

    test_markdown_html(original, expected, false, false, false);
}

#[test]
fn math_test_6() {
    let original = r##"$5x + 2 =
17$

$$\left( \sum_{k=1}^n a_k b_k \right)^2 \leq \left( \sum_{k=1}^n a_k^2 \right)
\left( \sum_{k=1}^n b_k^2 \right)$$
"##;
    let expected = r##"<p><span class="math inline">5x + 2 =
17</span></p>
<p><span class="math display">\left( \sum_{k=1}^n a_k b_k \right)^2 \leq \left( \sum_{k=1}^n a_k^2 \right)
\left( \sum_{k=1}^n b_k^2 \right)</span></p>
"##;

    test_markdown_html(original, expected, false, false, false);
}

#[test]
fn math_test_7() {
    let original = r##"$not a\
hard break  
either$
"##;
    let expected = r##"<p><span class="math inline">not a\
hard break  
either</span></p>
"##;

    test_markdown_html(original, expected, false, false, false);
}

#[test]
fn math_test_8() {
    let original = r##"$\$$

$$y = \$ x$$
"##;
    let expected = r##"<p><span class="math inline">\$</span></p>
<p><span class="math display">y = \$ x</span></p>
"##;

    test_markdown_html(original, expected, false, false, false);
}

#[test]
fn math_test_9() {
    let original = r##"$x $ x$

$$ $ $$

$$ $$ $$
"##;
    let expected = r##"<p>$x $ x$</p>
<p><span class="math display"> $ </span></p>
<p><span class="math display"> </span> $$</p>
"##;

    test_markdown_html(original, expected, false, false, false);
}

#[test]
fn math_test_10() {
    let original = r##"these are not math texts: $ y=x$, $y=x $, $
y=x$ and $y=x
$
"##;
    let expected = r##"<p>these are not math texts: $ y=x$, $y=x $, $
y=x$ and $y=x
$</p>
"##;

    test_markdown_html(original, expected, false, false, false);
}

#[test]
fn math_test_11() {
    let original = r##"these are math texts: foo$y=x$bar and $y=x$bar and foo$y=x$ bar
"##;
    let expected = r##"<p>these are math texts: foo<span class="math inline">y=x</span>bar and <span class="math inline">y=x</span>bar and foo<span class="math inline">y=x</span> bar</p>
"##;

    test_markdown_html(original, expected, false, false, false);
}

#[test]
fn math_test_12() {
    let original = r##"math texts: $x=y$! and $x=y$? and $x=y$: and $x=y$. and $x=y$"

also math texts: !$x=y$! and ?$x=y$? and :$x=y$: and .$x=y$. and "$x=y$"

braces: ($x=y$) [$x=y$] {$x=y$}
"##;
    let expected = r##"<p>math texts: <span class="math inline">x=y</span>! and <span class="math inline">x=y</span>? and <span class="math inline">x=y</span>: and <span class="math inline">x=y</span>. and <span class="math inline">x=y</span>&quot;</p>
<p>also math texts: !<span class="math inline">x=y</span>! and ?<span class="math inline">x=y</span>? and :<span class="math inline">x=y</span>: and .<span class="math inline">x=y</span>. and &quot;<span class="math inline">x=y</span>&quot;</p>
<p>braces: (<span class="math inline">x=y</span>) [<span class="math inline">x=y</span>] {<span class="math inline">x=y</span>}</p>
"##;

    test_markdown_html(original, expected, false, false, false);
}

#[test]
fn math_test_13() {
    let original = r##"$x=y$
"##;
    let expected = r##"<p><span class="math inline">x=y</span></p>
"##;

    test_markdown_html(original, expected, false, false, false);
}

#[test]
fn math_test_14() {
    let original = r##"$a$$b$

$a$$$b$$

$$a$$$b$

$$a$$$$b$$
"##;
    let expected = r##"<p><span class="math inline">a</span><span class="math inline">b</span></p>
<p><span class="math inline">a</span><span class="math display">b</span></p>
<p><span class="math display">a</span><span class="math inline">b</span></p>
<p><span class="math display">a</span><span class="math display">b</span></p>
"##;

    test_markdown_html(original, expected, false, false, false);
}

#[test]
fn math_test_15() {
    let original = r##"$Inline `first$ then` code

`Code $first` then$ inline

$$ Display `first $$ then` code

`Code $$ first` then $$ display
"##;
    let expected = r##"<p><span class="math inline">Inline `first</span> then` code</p>
<p><code>Code $first</code> then$ inline</p>
<p><span class="math display"> Display `first </span> then` code</p>
<p><code>Code $$ first</code> then $$ display</p>
"##;

    test_markdown_html(original, expected, false, false, false);
}

#[test]
fn math_test_16() {
    let original = r##"$x + y - z$

$x + y
- z$

$$ x + y
> z $$
"##;
    let expected = r##"<p><span class="math inline">x + y - z</span></p>
<p>$x + y</p>
<ul>
<li>z$</li>
</ul>
<p>$$ x + y</p>
<blockquote>
<p>z $$</p>
</blockquote>
"##;

    test_markdown_html(original, expected, false, false, false);
}
