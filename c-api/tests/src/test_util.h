#ifndef TEST_UTIL_H
#define TEST_UTIL_H

#include <stdlib.h>
#include <string.h>

#include "../../include/pulldown-cmark.h"

#define c_str_eq(actual, expected) ok(!strcmp(actual, expected))

#endif // TEST_UTIL_H
