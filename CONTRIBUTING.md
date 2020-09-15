Want to contribute? Great! First, read this page.

### Before you contribute

Before you start working on a larger contribution, you should get in touch with
us first through the issue tracker with your idea so that we can help out and
possibly guide you. Coordinating up front makes it much easier to avoid
frustration later on.

### Getting familiar with the project

**The architecture** is somewhat unique; it was originally inspired by [XML pull parsers](http://www.xmlpull.org), but ended up going in somewhat of its own direction. To get familiar with it, 
- start by reading the [README](README.md) page, which gives some details on the design of the parser (pull-based events) and some rationalization for it;
- read the [blog post](https://fullyfaithful.eu/pulldown-cmark) about the release of pulldown-cmark 0.3 by Marcus Klaas de Vries.

**The source code** can be approached by skimming the [API documentation](https://docs.rs/pulldown-cmark/latest/pulldown_cmark) first, then explore the code for the main struct, [`Parser`](https://docs.rs/pulldown-cmark/latest/pulldown_cmark/struct.Parser.html)

### Code reviews

All submissions, including submissions by project members, require review. We
use GitHub pull requests for this purpose.
