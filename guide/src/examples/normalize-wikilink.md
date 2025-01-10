Normalizes wikilinks as they pass through the parser.

The resulting destination url of the wikilink is normalized closely to how
[MediaWiki](https://www.mediawiki.org/wiki/Help:Links) links are normalized.
Use this example to customize this behavior for your use cases.

```rust
{{#include ../../../pulldown-cmark/examples/normalize-wikilink.rs}}
```
