//! This is a fuzzer for pulldown-cmark, which tries to detect super-linear parsing behaviour.
//! Additionally it can detect panicks of pulldown-cmark.
//!
//! The general idea is to extract relevant literals from the pulldown-cmark source code
//! (see the `literals` module).
//! Short random combinations of literals are generated (like concatenating 6 random literals),
//! which are called patterns.
//! Those patterns are repeated until they reach a byte-length of `NUM_BYTES`.
//! We iterate over subslices of the pattern to get `SAMPLE_SIZE` uniformly distributed
//! (len, time)-pairs.
//! Those pairs are fed into a scoring function to test if they seem to be linear or might be
//! super-linear (see the `scoring` module).
//! All of this is performed in parallel on multiple threads.
//!
//! Unfortunately we don't live in a perfect world, where linear patterns produce a perfect line.
//! instead we need to deal with large outliers, e.g. if a thread is rescheduled while performing
//! time measurements.
//! Therefore, we have a sort-of two pass system.
//! The first pass doesn't perform any outlier detection / handling.
//! This is because outlier handling is pretty slow.
//! The first pass just assumes there aren't any outliers, calculates `SAMPLE_SIZE` points and
//! applies the scoring functions to them.
//! This filters out most patterns already.
//! If however possible non-linear behaviour is found, we perform the second pass.
//! In the second pass, we double the `SAMPLE_SIZE`, because more points make for higher precision
//! in the scoring functions.
//! Additionally, we assume that the (len, time)-pairs interpreted as coordinates on a graph  are
//! strictly monotonously increasing.
//! If we encounter a time-value, which is larger than the next time value, that'll probably be an
//! outlier.
//! As such, if we encounter such a value, we'll re-time it.
//! Unfortunately, again, we have an edge-case.
//! When testing substrings of the input, we might not always end up after the whole pattern,
//! but instead in the middle of it.
//! Sometimes pulldown-cmark parses things differently if the strings ends differently.
//! For one example see <https://github.com/raphlinus/pulldown-cmark/issues/287>.
//! Thus we also need to detect these case.
//! Therefore we only remove a maximum of 10 outliers.
//! If more outliers are detected, measurements are aborted and an according message is printed.
//!
//! Additionally, we abort if parsing took longer than `MAX_MILLIS`.
//! If we tested a substring of the whole input, and already exceeded `MAX_MILLIS`, we shouldn't
//! continue testing with longer inputs, as we might have super-linear behaviour.
//! Instead, we again print an according message and abort.

use std::time::{Duration, Instant};
use std::panic;
use std::sync::atomic::{AtomicU64, Ordering};
use std::env;
use std::io::{self, BufRead};
use std::iter;

use itertools::Itertools;
use pulldown_cmark::{Parser, Options};
use rand::{SeedableRng, distributions::{Distribution, Uniform}};
use rand_xoshiro::Xoshiro256Plus;
use crossbeam_utils::thread;

mod literals;
mod black_box;
mod scoring;
mod clock;

use clock::{Clock, ThreadCpuTime};

/// After how many patterns each threads prints status reports including throughput.
const BATCH_SIZE: usize = 10_000;
/// Length of number of literals per combination.
///
/// For example given the literals `a` and `b`, with a value of 4, `aaab` is a possible pattern.
const COMBINATIONS: usize = 6;
/// If parsing takes longer than this many milliseconds, it is assumed to be non-linear and
/// parsing is aborted.
const MAX_MILLIS: u128 = 500;
/// Byte-length of maximum pattern repetitions used as input to pulldown-cmark.
const NUM_BYTES: usize = 256*1024;
/// Number of samples per pattern tested. Higher number increases precision, but reduces the throughput.
const SAMPLE_SIZE: usize = 5;
/// Score function to use when figuring out if something is non-linear.
///
/// Possible values: `scoring::{slope_stddev,pearson_correlation}`
const SCORE_FUNCTION: fn(&[(f64, f64)]) -> (f64, bool) = scoring::slope_stddev;
/// If slope_stddev is used, if the standard deviation is larger than this, it's assumed to be non-linear.
const ACCEPTANCE_STDDEV: f64 = 20.0;
/// If pearson_correlation is used, if the correlation coefficient is below this value,
/// it's assumed to be non-linear.
const ACCEPTANCE_CORRELATION: f64 = 0.995;
/// 0 / 1 / 2 / 3
const DEBUG_LEVEL: u8 = 0;

fn main() {
    let num_cpus = num_cpus::get() * 3 / 4;

    let arg = env::args().nth(1);
    match arg.as_ref().map(|s| s.as_str()) {
        Some("--retest") => {
            for pattern in io::stdin().lock().lines() {
                let pattern = pattern.unwrap();
                println!("retesting {:?}", pattern);
                let score = test(&pattern);
                println!("score: {}", score);
            }
        },

        Some("--regressions") => {
            // previously know cases
            regression_test();
        },
        _ => fuzz(num_cpus),
    }
}

/// Tests patterns of previously known super-linear parsing behaviour.
fn regression_test() {
    // https://github.com/raphlinus/pulldown-cmark/issues/246
    test("[](");
    // https://github.com/raphlinus/pulldown-cmark/issues/247
    test("``\\");
    // https://github.com/raphlinus/pulldown-cmark/issues/248
    test("a***");
    // https://github.com/raphlinus/pulldown-cmark/issues/249
    // TODO: we can't perform tests like this yet
//    test("* * * ...a");
    // https://github.com/raphlinus/pulldown-cmark/issues/251
    test("[ (](");
    // https://github.com/raphlinus/pulldown-cmark/issues/255
    test("[*_a");
    // https://github.com/raphlinus/pulldown-cmark/issues/280
    test("a <![CDATA[");
    // https://github.com/mity/md4c/issues/73#issuecomment-487640366
    test("a <!A");
    // https://github.com/raphlinus/pulldown-cmark/issues/282
    test("a<?");
    // https://github.com/raphlinus/pulldown-cmark/issues/284
    test("[[]()");
    // https://github.com/raphlinus/pulldown-cmark/issues/287
    // TODO: we can't perform tests like this reliably yet
//    test("[{}]:\\a");
    // https://github.com/raphlinus/pulldown-cmark/issues/296
    test("[](<");
}

/// Start fuzzing on given number of threads in parallel.
fn fuzz(num_cpus: usize) {
    let literals = literals::get();

    let num_batches_finished = AtomicU64::new(0);
    let num_batches_finished = &num_batches_finished;
    let mut rng = Xoshiro256Plus::from_rng(&mut rand::thread_rng()).unwrap();
    let start_time = Instant::now();

    // start threads
    thread::scope(|s| {
        let threads: Vec<_> = (0..num_cpus).map(|_| {
            let literals = literals.clone();
            // Get unique RNG for each thread. Read docs of Xoshiro256Plus for further information.
            rng.jump();
            let rng = rng.clone();
            s.spawn(move |_| worker_thread_fn(literals, &num_batches_finished, &start_time, rng))
        }).collect();

        for thread in threads {
            println!("Joined thread: {:?}", thread.join());
        }
    }).unwrap();
}

/// Function executed in worker-threads. Infinite loop generating and testing patterns.
fn worker_thread_fn(
    literals: Vec<Vec<u8>>, num_batches_finished: &AtomicU64, start_time: &Instant,
    mut rng: Xoshiro256Plus,
) {
    let uniform = Uniform::new(0, literals.len());
    loop {
        // generate `BATCH_SIZE` random patterns
        let chunks = iter::repeat_with(|| {
            &literals[uniform.sample(&mut rng)]
        }).chunks(COMBINATIONS);
        let combs = chunks.into_iter().take(BATCH_SIZE);

        // test those patterns, catching panics of pulldown-cmark (and our code)
        for comb in combs {
            let concatenated = comb.into_iter().flatten().cloned().collect::<Vec<_>>();
            // we mustn't panic here, because that would result in the thread being killed
            let pattern = match String::from_utf8(concatenated) {
                Ok(pattern) => pattern,
                Err(err) => {
                    println!("Couldn't convert pattern to UTF-8. Err: {:?}.", err);
                    continue;
                }
            };
            let res = panic::catch_unwind(|| {
                test(&pattern);
            });
            if res.is_err() {
                println!("Panic caught. Pattern: {:?}", pattern);
            }
        }

        // print status update and throughput

        let batches_finished = num_batches_finished.fetch_add(1, Ordering::Relaxed);
        // fetch_add returns the old value
        let batches_finished = batches_finished + 1;
        let patterns_finished = batches_finished * BATCH_SIZE as u64;
        let elapsed_secs = start_time.elapsed().as_secs();

        // if for some reason we are super-fast, we must avoid div-by-zero
        if elapsed_secs > 0 {
            println!(
                "{:<20} throughput: {} / s",
                patterns_finished,
                patterns_finished / elapsed_secs,
            );
        }
    }
}

/// Test a specific pattern, returning the score of the score-function.
///
/// This function prints its results to stdout.
/// No further handling is needed, the returned score is for debugging purposes mostly.
fn test(pattern: &str) -> f64 {
    let mut array = [(0.0, 0.0); SAMPLE_SIZE * 2];
    loop {
        // first pass, gets rid of most linear patterns if there aren't too many large outliers
        let res1 = test_pattern(pattern, &mut array[..SAMPLE_SIZE], false);
        if let PatternResult::Linear(score) = res1 {
            return score;
        }
        // If the first pass indicated possible non-linear behaviour, retest it with a larger
        // sample size and handle outliers.
        let res2 = test_pattern(pattern, &mut array, true);
        if let PatternResult::Linear(score) = res2 {
            return score;
        }

        // possible non-linear behaviour found

        let score = match res2 {
            PatternResult::Linear(..) => unreachable!(),
            PatternResult::NonLinear(score) => {
                println!(
                    "\n\
                    possible non-linear behaviour found\n\
                    pattern: {:?}\n\
                    score: {}\n\
                    {:?}\n",
                    pattern,
                    score,
                    array,
                );
                score
            },
            PatternResult::TooLong => {
                println!(
                    "\n\
                    possible non-linear behaviour found due to exceeding MAX_MILLIS (parsing took too long)\n\
                    pattern: {:?}\n\
                    score: 0\n\
                    {:?}\n",
                    pattern,
                    array,
                );
                0.0
            },
            PatternResult::HugeOutlier(trim_len) => {
                println!(
                    "\n\
                    huge outlier found, this may indicate weird behaviour in pulldown-cmark\n\
                    pattern: {:?}\n\
                    score: 0\n\
                    trim length: {}\n\
                    trimmed rest: {:?}\n\
                    {:?}\n",
                    pattern,
                    trim_len,
                    &pattern[..pattern.len() - trim_len],
                    array,
                );
                0.0
            },
        };
        return score;
    }
}

#[derive(Debug)]
enum PatternResult {
    /// Probably linear growth (score)
    Linear(f64),
    /// Probably non-linear growth (score)
    NonLinear(f64),
    /// Execution took too long, assumed non-linear
    TooLong,
    /// Very slow outlier, probably due to different parsing in pulldown-cmark.
    ///
    /// Contains the number of bytes truncated from end reaching this behaviour.
    /// Possibly indicates weird behaviour in pulldown-cmark.
    /// See <https://github.com/raphlinus/pulldown-cmark/issues/287> for an example.
    HugeOutlier(usize),
}

/// Tests a specific pattern, returning measurement and scoring outcomes.
fn test_pattern(pattern: &str, array: &mut [(f64, f64)], recalculate_outliers: bool) -> PatternResult {
    let sample_size = array.len();
    let s = pattern.repeat(NUM_BYTES / pattern.len());

    let mut i = 0;
    let mut huge_outliers = 0;
    while i < sample_size {
        let (n, dur) = time_needed(&s, i+1, sample_size);
        array[i] = (n as f64, dur.as_nanos() as f64);
        if DEBUG_LEVEL >= 3 {
            println!("duration: {}", dur.as_nanos());
        }

        if recalculate_outliers && i > 0 && array[i-1].1 > array[i].1 {
            // We have an outlier, possibly due to rescheduling.

            // We can have a consistent huge outlier due to weird parsing behaviour when parts of
            // the pattern are truncated at the end (see <https://github.com/raphlinus/pulldown-cmark/issues/287>).
            // In that case we don't want to end in an infinite loop.
            // Instead, if we encounter a huge outlier 10 times in the same test, we abort this pattern.
            if array[i-1].1 > array[i].1 * 50.0 {
                huge_outliers += 1;
                if huge_outliers > 10 {
                    let last_repetition_start = pattern.len() + s.rfind(pattern).unwrap();
                    let trim_len = pattern.len() - (s.len() - last_repetition_start);
                    return PatternResult::HugeOutlier(trim_len);
                }
            }

            // Redo from the last sample
            if DEBUG_LEVEL >= 3 {
                println!("removed outlier");
            }
            i -= 1;
            continue;
        }

        if dur.as_millis() > MAX_MILLIS {
            return PatternResult::TooLong;
        }
        i += 1;
    }

    let (score, non_linear) = SCORE_FUNCTION(&array);

    if DEBUG_LEVEL >= 1 {
        println!("{:<30}{}", score, pattern);
    }
    if DEBUG_LEVEL >= 2 {
        println!("{:?}", array);
    }

    if non_linear {
        PatternResult::NonLinear(score)
    } else {
        PatternResult::Linear(score)
    }
}

/// Returns the length of the tested substring and the time needed for parsing given string in
/// given sample of given sample_size.
///
/// The passed string is the whole repeated pattern string.
/// This function perform substring slicing according to the current sample and sample size uniformly.
fn time_needed(s: &str, sample: usize, sample_size: usize) -> (usize, Duration) {
    let mut n = s.len() / sample_size * sample;
    // find closest byte boundary
    while !s.is_char_boundary(n) {
        n += 1;
    }

    // perform actual time measurement
    let parser = Parser::new_ext(&s, Options::all());
    let clock = Clock::<ThreadCpuTime>::now();
    parser.for_each(|evt| {
        black_box::black_box(evt);
    });
    (n, clock.elapsed())
}


