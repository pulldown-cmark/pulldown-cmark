/*
 * Copyright (c) 2014 Jorge Aparicio
 * Licensed under the MIT license
 */

// stolen from criterion
// https://github.com/bheisler/criterion.rs/blob/c21de8397a1e879321ab4ffbf3d5a6b0997300c3/src/lib.rs#L157
pub fn black_box<T>(dummy: T) -> T {
    unsafe {
        let ret = std::ptr::read_volatile(&dummy);
        std::mem::forget(dummy);
        ret
    }
}
