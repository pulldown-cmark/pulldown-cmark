#include <string.h>
#include <stdio.h>

#include "../../include/pulldown-cmark.h"
#include "deps/picotest/picotest.h"
#include "test.h"
#include "test_util.h"

void test_commonmark_render() {
    const char* commonmark = "Hello *world*!";

    const char* html = pulldown_cmark_commonmark_to_html(commonmark, 0);
    const char* expected = "<p>Hello <em>world</em>!</p>\n";

    c_str_eq(html, expected);
}
