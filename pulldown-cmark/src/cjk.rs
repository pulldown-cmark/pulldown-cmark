// CJK-friendly emphasis support.
//
// This module implements character classification functions for the
// CommonMark CJK-friendly amendments specification:
// https://github.com/tats-u/markdown-cjk-friendly/blob/main/specification.md
//
// The key insight is that CJK punctuation (e.g. 「」、。！？（）) should not
// block delimiter flanking detection the way ASCII punctuation does, because
// CJK languages do not use spaces between words and punctuation.

use crate::puncttable::is_punctuation;

/// Returns true if `c` is a CJK character as defined by the CJK-friendly
/// amendments specification.
///
/// A CJK character has East Asian Width W (Wide), F (Fullwidth), or H (Halfwidth)
/// and is not a default emoji presentation character, OR has Unicode Script = Hangul.
///
/// The ranges are derived from Unicode 16.0 / markdown-cjk-friendly ranges.md.
pub(crate) fn is_cjk_character(c: char) -> bool {
    let cp = c as u32;
    matches!(cp,
        // Hangul Jamo
        0x1100..=0x11FF |
        // CJK Radicals Supplement, Kangxi Radicals
        0x2E80..=0x2FDF |
        // Ideographic Description Characters, CJK Symbols and Punctuation
        0x2FF0..=0x303F |
        // Hiragana
        0x3041..=0x3096 |
        0x3099..=0x309F |
        // Katakana
        0x30A0..=0x30FF |
        // Bopomofo
        0x3105..=0x312F |
        // Hangul Compatibility Jamo
        0x3131..=0x318E |
        // Kanbun, Bopomofo Extended
        0x3190..=0x31BF |
        // CJK Strokes
        0x31C0..=0x31EF |
        // Katakana Phonetic Extensions
        0x31F0..=0x31FF |
        // Enclosed CJK Letters and Months
        0x3200..=0x32FF |
        // CJK Compatibility
        0x3300..=0x33FF |
        // CJK Unified Ideographs Extension A
        0x3400..=0x4DBF |
        // CJK Unified Ideographs
        0x4E00..=0x9FFF |
        // Yi Syllables, Yi Radicals
        0xA000..=0xA4CF |
        // Hangul Jamo Extended-A
        0xA960..=0xA97F |
        // Hangul Syllables
        0xAC00..=0xD7A3 |
        // Hangul Jamo Extended-B
        0xD7B0..=0xD7FF |
        // CJK Compatibility Ideographs
        0xF900..=0xFAFF |
        // CJK Compatibility Forms, Small Form Variants
        0xFE30..=0xFE6F |
        // Halfwidth and Fullwidth Forms
        0xFF01..=0xFF60 |
        0xFFE0..=0xFFE6 |
        // Halfwidth Katakana
        0xFF65..=0xFF9F |
        // Halfwidth Hangul
        0xFFA0..=0xFFDC |
        // Halfwidth symbols
        0xFFE8..=0xFFEE |
        // Kana Extended-B, Kana Supplement, Kana Extended-A, Small Kana Extension
        0x1AFF0..=0x1B16F |
        // CJK Unified Ideographs Extension B
        0x20000..=0x2A6DF |
        // CJK Unified Ideographs Extension C
        0x2A700..=0x2B73F |
        // CJK Unified Ideographs Extension D
        0x2B740..=0x2B81F |
        // CJK Unified Ideographs Extension E
        0x2B820..=0x2CEAF |
        // CJK Unified Ideographs Extension F
        0x2CEB0..=0x2EBEF |
        // CJK Unified Ideographs Extension I
        0x2EBF0..=0x2F7FF |
        // CJK Compatibility Ideographs Supplement
        0x2F800..=0x2FA1F |
        // CJK Unified Ideographs Extension G
        0x30000..=0x3134F |
        // CJK Unified Ideographs Extension H
        0x31350..=0x323AF
    )
}

/// Returns true if `c` is a punctuation character that is NOT a CJK character.
///
/// In the CJK-friendly amendments, the distinction between CJK punctuation
/// and non-CJK punctuation is crucial: CJK punctuation should not prevent
/// delimiter runs from being flanking.
pub(crate) fn is_non_cjk_punctuation(c: char) -> bool {
    is_punctuation(c) && !is_cjk_character(c)
}

/// Returns true if `c` is a non-emoji general-use variation selector (U+FE00..U+FE0E).
///
/// These selectors can appear between a CJK character and a delimiter.
/// When checking the character before a delimiter, we need to "see through"
/// these selectors to find the actual preceding character.
///
/// Note: U+FE0F (Emoji Presentation Selector) is excluded.
pub(crate) fn is_general_variation_selector(c: char) -> bool {
    ('\u{FE00}'..='\u{FE0E}').contains(&c)
}

/// Returns true if `c` is an Ideographic Variation Selector (U+E0100..U+E01EF).
///
/// An IVS before a delimiter run means the preceding character is a CJK ideograph.
pub(crate) fn is_ideographic_variation_selector(c: char) -> bool {
    ('\u{E0100}'..='\u{E01EF}').contains(&c)
}

/// Look through variation selectors to find the "real" preceding character
/// and the character before that.
///
/// Returns (prev_char, prev_prev_char) after skipping variation selectors.
pub(crate) fn previous_chars_skip_vs(s: &str, ix: usize) -> (Option<char>, Option<char>) {
    let mut iter = s[..ix].chars().rev();
    // Skip variation selectors
    let mut prev = iter.next();
    while prev.is_some_and(|c| is_general_variation_selector(c) || is_ideographic_variation_selector(c)) {
        prev = iter.next();
    }
    let prev_prev = iter.next();
    (prev, prev_prev)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cjk_characters() {
        // Hiragana
        assert!(is_cjk_character('あ'));
        assert!(is_cjk_character('ん'));
        // Katakana
        assert!(is_cjk_character('ア'));
        assert!(is_cjk_character('ン'));
        // CJK Unified Ideographs
        assert!(is_cjk_character('漢'));
        assert!(is_cjk_character('字'));
        // Hangul
        assert!(is_cjk_character('한'));
        assert!(is_cjk_character('글'));
        // CJK punctuation
        assert!(is_cjk_character('「'));
        assert!(is_cjk_character('」'));
        assert!(is_cjk_character('。'));
        assert!(is_cjk_character('、'));
        // Fullwidth forms
        assert!(is_cjk_character('！'));
        assert!(is_cjk_character('？'));
        assert!(is_cjk_character('（'));
        assert!(is_cjk_character('）'));
        // Non-CJK
        assert!(!is_cjk_character('a'));
        assert!(!is_cjk_character('Z'));
        assert!(!is_cjk_character('.'));
        assert!(!is_cjk_character('!'));
        assert!(!is_cjk_character(' '));
    }

    #[test]
    fn test_non_cjk_punctuation() {
        // ASCII punctuation is non-CJK punctuation
        assert!(is_non_cjk_punctuation('.'));
        assert!(is_non_cjk_punctuation('!'));
        assert!(is_non_cjk_punctuation('('));
        assert!(is_non_cjk_punctuation(')'));
        // CJK punctuation is NOT non-CJK punctuation
        assert!(!is_non_cjk_punctuation('。'));
        assert!(!is_non_cjk_punctuation('、'));
        assert!(!is_non_cjk_punctuation('「'));
        assert!(!is_non_cjk_punctuation('」'));
        assert!(!is_non_cjk_punctuation('！'));
        assert!(!is_non_cjk_punctuation('？'));
        // Regular letters are not punctuation at all
        assert!(!is_non_cjk_punctuation('a'));
        assert!(!is_non_cjk_punctuation('漢'));
    }

    #[test]
    fn test_variation_selectors() {
        assert!(is_general_variation_selector('\u{FE00}'));
        assert!(is_general_variation_selector('\u{FE0E}'));
        assert!(!is_general_variation_selector('\u{FE0F}')); // emoji presentation selector excluded
        assert!(is_ideographic_variation_selector('\u{E0100}'));
        assert!(is_ideographic_variation_selector('\u{E01EF}'));
        assert!(!is_ideographic_variation_selector('\u{E01F0}'));
    }
}
