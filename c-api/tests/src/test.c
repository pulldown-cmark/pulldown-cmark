#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "deps/picotest/picotest.h"
#include "../../include/pulldown-cmark.h"

#include "test.h"

int run_tests() {
    subtest("Commonmark render", test_commonmark_render);

    return done_testing();
}
