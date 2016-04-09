# fancy-regex

This module contains a hybrid regex implementation, designed to
support a relatively rich set of features. In particular, this
implementation uses backtracking to implement "fancy" features such as
look-around and backtracking, which are not supported in purely NFA-
based implementations (exemplified by
[RE2](https://github.com/google/re2), and implemented in Rust in the
[regex](https://crates.io/crates/regex) crate).

A goal is to be as efficient as possible. For a given regex, the NFA
implementation has asymptotic running time linear in the length of the
input, while in the general case a backtracking implementation has
exponential blowup. A an example given in [Static Analysis for Regular
Expression Exponential Runtime via Substructural
Logics](https://www.cs.bham.ac.uk/~hxt/research/redos_full.pdf) is:

```python import re re.compile('(a|b|ab)*bc').match('ab' * 28 + 'ac')
```

In Python 2.7, this match takes 91s, and doubles for each additional
repeat of 'ab'.

Thus, many proponents
[advocate](https://swtch.com/~rsc/regexp/regexp1.html) a purely NFA
based approach. Even so, backreferences and look-around do add
richness to regexes, and they are commonly used in applications such
as syntax highlighting for text editors. In particular, TextMate's
[syntax
definitions](https://manual.macromates.com/en/language_grammars),
based on the [Oniguruma](https://github.com/kkos/oniguruma)
backtracking engine, are now used in a number of other popular
editors, including Sublime Text and Atom. These syntax definitions
routinely use backreferences and look-around. For example, the
following regex captures a single-line Rust raw string:

``` r(#*)".*?"\1 ```

There is no NFA that can express this simple and useful pattern. Yet,
a backtracking implementation handles it efficiently.

This package is one of the first that handles both cases well. The
exponential blowup case above is run in 258ns. Thus, it should be a
very appealing alternative for applications that require both richness
and performance.

## A warning about worst-case performance

NFA-based approaches give strong guarantees about worst-case
performance. For regexes that contain "fancy" features such as
backreferences and look-around, this module gives no corresponding
guarantee. If an attacker can control the regular expressions that
will be matched against, they will be able to successfully mount a
denial-of-service attack. Be warned.

## A hybrid approach

One workable approach is to detect the presence of "fancy" features,
and choose either an NFA implementation or a backtracker depending on
whether they are used.

However, this module attempts to be more fine-grained. Instead, it
implements a true hybrid approach. In essence, it is a backtracking VM
(as well explained in [Regular Expression Matching: the Virtual
Machine Approach](https://swtch.com/~rsc/regexp/regexp2.html)) in
which one of the "instructions" in the VM delegates to an inner NFA
implementation (in Rust, the regex crate, though a similar approach
would certainly be possible using RE2 or the Go
[regexp](https://golang.org/pkg/regexp/) package). Then there's an
analysis which decides for each subexpression whether it is "hard", or
can be delegated to the NFA matcher. At the moment, it is eager, and
delegates as much as possible to the NFA engine.

## Theory

TODO: write this

## Current status

Still in development, though the basic ideas are in place. Currently,
the following features are missing:

* A proper API for actually using the engien.

* Correct handling of zero-width matchers such as `\b` that require
one character of look-behind.

* Plumbing of captures from the NFA inner matcher to the client.

* The following regex language features not yet implemented:

     * Look-behind

     * Atomic groups

     * Procedure calls and recursive expressions

     * Correct handling of flags

## Acknowledgements

Many thanks to [Andrew Gallant](http://blog.burntsushi.net/about/) for
stimulating conversations that inspired this approach, as well as for
creating the excellent regex crate.

