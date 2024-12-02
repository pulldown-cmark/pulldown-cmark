# Performance Optimizations

This chapter covers the key performance optimizations implemented in pulldown-cmark. The library uses several techniques to achieve fast Markdown parsing while maintaining standards compliance and a clean architecture.

## SIMD-Accelerated Character Scanning

One of the most performance-critical operations in Markdown parsing is scanning text for special characters that may indicate inline markup. pulldown-cmark uses SIMD (Single Instruction Multiple Data) instructions on x86_64 platforms to accelerate this scanning.

The SIMD optimization is implemented in `scanners.rs` and operates by:

1. Creating a lookup table of special characters (like `*`, `_`, etc.)
2. Loading 16 bytes at a time into a SIMD register
3. Performing parallel lookups to identify special characters
4. Generating a bitmask indicating which bytes matched

```rust
// Example from firstpass.rs showing the core scanning logic
#[target_feature(enable = "ssse3")]
unsafe fn compute_mask(lut: &[u8; 16], bytes: &[u8], ix: usize) -> i32 {
    let bitmap = _mm_loadu_si128(lut.as_ptr() as *const __m128i);
    let input = _mm_loadu_si128(bytes.as_ptr().add(ix) as *const __m128i);
    let bitset = _mm_shuffle_epi8(bitmap, input);
    let higher_nibbles = _mm_and_si128(_mm_srli_epi16(input, 4), _mm_set1_epi8(0x0f));
    let bitmask = _mm_shuffle_epi8(bitmask_lookup, higher_nibbles);
    let tmp = _mm_and_si128(bitset, bitmask);
    let result = _mm_cmpeq_epi8(tmp, bitmask);
    _mm_movemask_epi8(result)
}
```

This SIMD optimization can provide significant speedups when processing large documents, since character scanning is such a common operation. The code falls back to scalar processing when SIMD is not available.

## Memory-Efficient String Storage

The library uses a custom string type `CowStr` that can represent strings. Refer to the [string handling](./string-handling.md) documentation for more details on the performance optimizations inherent to this type.

## Tree Structure Optimization

The AST (Abstract Syntax Tree) is stored in a vec-based tree structure that provides:

1. Fast node creation during parsing
2. Efficient tree traversal
3. Memory locality from vector storage

```rust
pub(crate) struct Tree<T> {
    nodes: Vec<Node<T>>,
    spine: Vec<TreeIndex>,
    cur: Option<TreeIndex>, 
}

pub(crate) struct Node<T> {
    pub child: Option<TreeIndex>,
    pub next: Option<TreeIndex>,
    pub item: T,
}
```

Key optimizations in the tree structure include:

- Using indices instead of pointers for node references
- Maintaining a "spine" for fast access to ancestor nodes
- Storing nodes contiguously in a vector for better cache usage
- Using non-zero indices to save space in option types

## Protection Against Pathological Input

It is important that the parser performance remain linear with respect to the input length, otherwise the parser would find itself vulnerable to potential DOS attacks. This may not be important for all consumers, but for anyone depending on the library to handle user generated content this is critical.

Several protections are in place to prevent quadratic time or memory usage on malicious input:

1. Link nesting depth is limited:
```rust
pub(crate) const LINK_MAX_NESTED_PARENS: usize = 32;
```

2. Table column expansion is bounded:
```rust
// Limit to prevent quadratic growth from empty cells
const MAX_AUTOCOMPLETED_CELLS: usize = 1 << 18;
```

3. Link reference expansion tracking:
```rust
// Track expansion to prevent quadratic growth from reference definitions
let mut link_ref_expansion_limit: usize = text.len().max(100_000);
```


## Key Performance Considerations

When using the library, keep in mind:

1. SIMD optimizations require the `simd` feature and x86_64 platform
2. Large documents benefit most from SIMD scanning
4. The parser is designed for streaming, allowing incremental processing
5. Pathological input protection may limit processing of extremely nested or repetitive content

## Benchmarking

The library includes benchmarks to measure performance of key operations:

- String handling with different storage strategies
- Tree operations
- Full document parsing
- Pathological input cases

When making changes that could affect performance, run the benchmarks to ensure optimizations are effective:

```bash
cargo bench
```

Note that some optimizations (like SIMD) are platform-specific, so testing on multiple platforms may be necessary.
