#ifndef PULLDOWN_CMARK_H
#define PULLDOWN_CMARK_H

#if defined(__cplusplus)
extern "C" {
#endif

#if defined(__cplusplus)
}  // extern C
#endif

/** Convert 'text' (assumed to be a UTF-8 encoded string) from CommonMark Markdown to HTML,
 * returning a null-terminated, UTF-8-encoded string.
 *
 * It is the caller's responsibility to free the returned buffer.
 */
char *pulldown_cmark_commonmark_to_html(const char *text, int options);

#endif // PULLDOWN_CMARK_H
