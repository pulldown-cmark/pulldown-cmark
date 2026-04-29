# Demo

<style>
#form {
  * {
    box-sizing: border-box;
  }
  .main {
    textarea {
      width: 100%;
    }
    @media (width >= 600px) {
      display: flex;
      flex-flow: row nowrap;
      > * {
        flex: 0 0 50%;
      }
    }
    > .output {
      /* Use sidebar colors for the tabs to get something that is in the right colour scheme. */
      > .tabs {
        background: var(--sidebar-bg);
        color: var(--sidebar-fg);
        input {
          display: none;
        }
        > label {
          display: inline-block;
          padding: 0.3em 0.5em;
          cursor: pointer;
        }
        > label:has(> input:checked) {
          background: var(--sidebar-fg);
          color: var(--sidebar-bg);
        }
        > label:not(:has(> input:checked)) {
          &:hover {
            color: var(--sidebar-active);
          }
        }
      }
      display: flex;
      flex-flow: column nowrap;
      > output {
        flex: 1 1 auto;
        > * {
          display: none;
          width: 100%;
          height: 100%;
        }
        > pre {
          margin: 0;
          white-space: pre-wrap;
        }
      }
      &:has(input[name="format"][value="rendered"]:checked) .rendered {
        display: block;
      }
      &:has(input[name="format"][value="html"]:checked) .html {
        display: block;
      }
      &:has(input[name="format"][value="ast"]:checked) .ast {
        display: block;
      }
    }
  }
  fieldset[name="options"] {
    margin-top: 1em;
    > div {
      column-width: 12em;
      > label {
        display: block;
      }
    }
  }
}
</style>

<form id="form">
  <div class="main">

<!--
Create a new HTML block to prevent the default Markdown from being parsed by mdBook as Markdown.
-->
<textarea rows="16" name="md" autofocus placeholder="Markdown here!">
## Try pulldown-cmark

You can try [pulldown-cmark](https://github.com/pulldown-cmark/pulldown-cmark) here.
This demo is powered by WebAssembly.

1. item one
2. item two
   - sublist
   - sublist
</textarea>

<div class="output">
      <div class="tabs">
        <label><input type="radio" name="format" value="rendered" checked>Rendered</label>
        <label><input type="radio" name="format" value="html">HTML</label>
        <label><input type="radio" name="format" value="ast">AST</label>
      </div>
      <output>
        <iframe src="preview.html" class="rendered" frameborder="0" sandbox="allow-same-origin"></iframe>
        <pre class="html"><code class="language-html"></code></pre>
        <div class="ast"></div>
      </output>
    </div>
  </div>
  <button type="button" class="clear">Clear</button>
  <button type="button" class="permalink">Copy permalink</button>
  <fieldset name="options">
    <legend>
      <a href="https://docs.rs/pulldown-cmark/latest/pulldown_cmark/struct.Options.html"><code>Options</code></a>
    </legend>
    <div>
      <label><input type="checkbox" name="option-1">Tables</label>
      <label><input type="checkbox" name="option-2">Footnotes</label>
      <label><input type="checkbox" name="option-3">Strikethrough</label>
      <label><input type="checkbox" name="option-4">Tasklists</label>
      <label><input type="checkbox" name="option-5">Smart punctuation</label>
      <label><input type="checkbox" name="option-6">Heading attributes</label>
      <label><input type="checkbox" name="option-7">YAML-style metadata blocks</label>
      <label><input type="checkbox" name="option-8">Pluses-delimited metadata blocks</label>
      <label><input type="checkbox" name="option-9">Old footnotes</label>
      <label><input type="checkbox" name="option-10">Math</label>
      <label><input type="checkbox" name="option-11">GFM</label>
      <label><input type="checkbox" name="option-12">Definition lists</label>
      <label><input type="checkbox" name="option-13">Superscript</label>
      <label><input type="checkbox" name="option-14">Subscript</label>
      <label><input type="checkbox" name="option-15">Wikilinks</label>
    </div>
  </fieldset>
</form>

<script type="module">
const { instance } = await WebAssembly.instantiateStreaming(fetch("demo.wasm"));

const form = document.getElementById("form");

// Ensure that the "footnotes" and "old footnotes" options stay in sync (the latter cannot be
// enabled without the former).
{
  const footnotes = form.elements["option-2"];
  const oldFootnotes = form.elements["option-9"];

  footnotes.addEventListener("input", () => {
    if (!footnotes.checked) {
      oldFootnotes.checked = false;
    }
  });
  oldFootnotes.addEventListener("input", () => {
    if (oldFootnotes.checked) {
      footnotes.checked = true;
    }
  });
}

// mdBook adds a document-level keydown listener for the arrow keys for page navigation. This
// interferes with editing the textarea, so we stop propagation of these keydown events.
form.elements["md"].addEventListener("keydown", e => e.stopPropagation());

const outputRendered = form.querySelector(".rendered").contentDocument.body;
const outputHtml = form.querySelector(".html > code");
const outputAst = form.querySelector(".ast");

const readOptions = () => {
  let options = 0;
  for (const option of form.elements["options"].elements) {
    if (!option.checked) {
      continue;
    }
    options |= 1 << parseInt(option.name.slice("option-".length));
  }
  return options;
};

const update = () => {
  const md = form.elements["md"].value;
  const format = form.elements["format"].value;
  const options = readOptions();

  // First, allocate the Markdown as a string inside WASM memory.
  const mdCap = md.length * 3; // Every UTF-16 code unit results in at most 3 UTF-8 bytes
  const mdPtr = mdCap === 0 ? 1 : instance.exports.alloc(mdCap, 1);
  const { written: mdLen } =
    new TextEncoder().encodeInto(md, new Uint8Array(instance.exports.memory.buffer, mdPtr));

  // Allocate space for a (ptr, len, cap) tuple, where the output string is stored.
  const out = instance.exports.alloc(4 * 3, 4);

  // Render the Markdown.
  instance.exports.render(mdPtr, mdLen, mdCap, options, format === "ast", out);
  const [renderedPtr, renderedLen, renderedCap] =
    new Uint32Array(instance.exports.memory.buffer, out);
  instance.exports.dealloc(out, 4 * 3, 4);

  // Convert the rendered content to a Javascript string and deallocate.
  const rendered = new TextDecoder().decode(
    new Uint8Array(instance.exports.memory.buffer, renderedPtr, renderedLen),
  );
  if (renderedCap !== 0) instance.exports.dealloc(renderedPtr, renderedCap, 1);

  if (format === "rendered") {
    outputRendered.setHTMLUnsafe(rendered);
  } else if (format === "html") {
    outputHtml.textContent = rendered;

    // In highlight.js v11, highlightBlock was renamed to highlightElement. mdBook uses v10 at the
    // time of writing (mdBook v0.4.43), but to avoid breaking when it updates, we use whichever is
    // available.
    (hljs.highlightElement ?? hljs.highlightBlock)(outputHtml);
  } else if (format === "ast") {
    outputAst.setHTMLUnsafe(rendered);
  }
};
form.addEventListener("input", update);

form.querySelector("button.clear").addEventListener("click", () => {
  form.elements["md"].value = "";
  update();
});

form.querySelector("button.permalink").addEventListener("click", () => {
  const url = new URL(location.href);
  url.search = "";
  url.searchParams.append("md", form.elements["md"].value);
  url.searchParams.append("format", form.elements["format"].value);
  url.searchParams.append("options", readOptions());
  history.pushState(null, "", url.href);
  navigator.clipboard.writeText(url.href);
});

{
  const params = new URLSearchParams(location.search);
  const md = params.get("md");
  const format = params.get("format");
  const optionsStr = params.get("options");
  if (md !== null) {
    form.elements["md"].value = md;
  }
  if (format !== null) {
    form.elements["format"].value = format;
  }
  if (optionsStr !== null) {
    const options = parseInt(optionsStr);
    for (const option of form.elements["options"].elements) {
      option.checked = (options & (1 << parseInt(option.name.slice("option-".length)))) !== 0;
    }
  }
}

update();
</script>
