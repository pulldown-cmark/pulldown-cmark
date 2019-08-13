//! This is a fuzzer for pulldown-cmark, which tries to detect super-linear parsing behaviour.
//! Additionally it can detect panicks of pulldown-cmark.
//!
//! The general idea is to extract relevant literals from the pulldown-cmark source code
//! (see the `literals` module).
//! Short random combinations of literals are generated (like concatenating 6 random literals),
//! which are called patterns.
//! Those patterns are repeated until they reach a byte-length of `NUM_BYTES`.
//! We iterate over subslices of the pattern to get `SAMPLE_SIZE` evenly distributed
//! (len, time)-pairs.
//! Those pairs are fed into a scoring function to test if they seem to be linear or might be
//! super-linear (see the `scoring` module).
//! All of this is performed in parallel on multiple threads.
//!
//! Unfortunately we don't live in a perfect world, where linear patterns produce a perfect line.
//! instead we need to deal with large outliers, e.g. if a thread is rescheduled while performing
//! time measurements. Therefore, we retest positive hits up to `TEST_COUNT` times to make
//! false positives less likely. Only when all of these tests are positive a pattern is flagged as
//! superlinear.
//! To prevent further edge-cases, we always align the tested input to pattern boundaries.
//!
//! Additionally, we abort if parsing took longer than `MAX_MILLIS`.
//! If we tested a substring of the whole input, and already exceeded `MAX_MILLIS`, we shouldn't
//! continue testing with longer inputs, as we might have super-linear behaviour.
//! Instead, we again print an according message and abort with that pattern.
//!
//! There can also be cases where either the fuzzer or the parser takes a very long time testing
//! a single pattern or ends up in an infinite loop.
//! If the time a thread spends on a single pattern exceeds `10 * MAX_MILLIS`, it could indicate
//! bugs in the fuzzer, extremely long parsing of an input, or maybe even an infinite loop.
//! As such we report the pattern the thread currently works on.
//! To kill that thread and not let it be stuck in a possibly inifinite loop forever, we restart
//! the whole process (by executing `exec` with our own binary).
//! While there are other ways of killing threads / handling infinite loops, as evaluated in
//! <https://github.com/raphlinus/pulldown-cmark/pull/286#issuecomment-490315582>,
//! the easiest solution is to just restart the whole process.

use std::env;
use std::iter;
use std::os::unix::process::CommandExt;
use std::panic;
use std::process::Command;
use std::sync::{
    atomic::{AtomicU64, Ordering},
    Mutex,
};
use std::time::{Duration, Instant};

use crossbeam_utils::thread;
use pulldown_cmark::{Options, Parser};
use rand::{distributions::Distribution, seq::SliceRandom, Rng, SeedableRng};
use rand_xoshiro::Xoshiro256Plus;

mod black_box;
mod clock;
mod literals;
mod scoring;

use clock::{Clock, ThreadCpuTime};

/// After how many patterns each threads prints status reports including throughput.
const BATCH_SIZE: usize = 10_000;
/// Length of number of literals per combination.
///
/// For example given the literals `a` and `b`, with a value of 4, `aaab` is a possible pattern.
const COMBINATIONS: usize = 6;
/// If parsing takes longer than this many milliseconds, it is assumed to be superlinear and
/// parsing is aborted.
const MAX_MILLIS: u128 = 500;
/// Byte-length of maximum pattern repetitions used as input to pulldown-cmark.
const NUM_BYTES: usize = 8 * 1024;
/// Number of samples per pattern tested. Higher number increases precision, but reduces the throughput.
const SAMPLE_SIZE: usize = 5;
/// Score function to use when figuring out if something is non-linear.
///
/// Possible values: `scoring::{slope_stddev,pearson_correlation}`
const SCORE_FUNCTION: fn(&[(f64, f64)]) -> (f64, bool) = scoring::slope_stddev;
/// If slope_stddev is used, if the standard deviation is larger than this, it's assumed to be non-linear.
const ACCEPTANCE_STDDEV: f64 = 30.0;
/// If pearson_correlation is used, if the correlation coefficient is below this value,
/// it's assumed to be non-linear.
const ACCEPTANCE_CORRELATION: f64 = 0.995;
/// Number of times we test until we are convinced of superlinear behavior
const TEST_COUNT: usize = 5;
/// 0 / 1 / 2 / 3
const DEBUG_LEVEL: u8 = 0;

#[derive(Debug, Clone, Default)]
struct Pattern {
    prefix: String,
    repeating_pattern: String,
    postfix: String,
}

impl<'a> Into<Pattern> for &'a str {
    fn into(self) -> Pattern {
        Pattern {
            prefix: String::new(),
            repeating_pattern: self.to_owned(),
            postfix: String::new(),
        }
    }
}

struct UniformPatterns<'a> {
    patterns: &'a [Vec<u8>],
}

impl<'a> UniformPatterns<'a> {
    fn new(patterns: &'a [Vec<u8>]) -> Self {
        Self { patterns }
    }
}

impl<'a> Distribution<Pattern> for UniformPatterns<'a> {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Pattern {
        fn random_seq<'b, I: Iterator<Item = Option<&'b Vec<u8>>>>(iter: &mut I) -> String {
            String::from_utf8(
                iter.take(COMBINATIONS)
                    .flatten()
                    .flatten()
                    .cloned()
                    .collect(),
            )
            .expect("Bytes to String failed!")
        }

        let mut random_words = iter::repeat_with(|| self.patterns.choose(rng));

        Pattern {
            prefix: random_seq(&mut random_words),
            postfix: random_seq(&mut random_words),
            repeating_pattern: random_seq(&mut random_words),
        }
    }
}

fn main() {
    let num_cpus = (num_cpus::get() as f32 * 0.8).ceil() as usize;

    let arg = env::args().nth(1);
    match arg.as_ref().map(|s| s.as_str()) {
        Some("--regressions") => {
            // previously know cases
            let exit_code = regression_test();
            std::process::exit(exit_code);
        }
        _ => fuzz(num_cpus),
    }
}

/// Tests patterns of previously known super-linear parsing behaviour.
///
/// Returns the exit code. 0 if all tests passed and 1 otherwise.
fn regression_test() -> i32 {
    let mut exit_code = 0;
    let mut check_pattern = |pat| match test(&pat) {
        PatternResult::Linear(_) => (),
        _ => exit_code = 1,
    };
    // https://github.com/raphlinus/pulldown-cmark/issues/246
    check_pattern("[](".into());
    // https://github.com/raphlinus/pulldown-cmark/issues/247
    check_pattern("``\\".into());
    // https://github.com/raphlinus/pulldown-cmark/issues/248
    check_pattern("a***".into());
    // https://github.com/raphlinus/pulldown-cmark/issues/249
    check_pattern(Pattern {
        prefix: "".into(),
        repeating_pattern: "* ".into(),
        postfix: "a".into(),
    });
    // https://github.com/raphlinus/pulldown-cmark/issues/251
    check_pattern("[ (](".into());
    // https://github.com/raphlinus/pulldown-cmark/issues/255
    check_pattern("[*_a".into());
    // https://github.com/raphlinus/pulldown-cmark/issues/280
    check_pattern("a <![CDATA[".into());
    // https://github.com/mity/md4c/issues/73#issuecomment-487640366
    check_pattern("a <!A".into());
    // https://github.com/raphlinus/pulldown-cmark/issues/282
    check_pattern("a<?".into());
    // https://github.com/raphlinus/pulldown-cmark/issues/284
    check_pattern("[[]()".into());
    // https://github.com/raphlinus/pulldown-cmark/issues/296
    check_pattern("[](<".into());
    check_pattern("[\"[]]\\(".into());
    check_pattern(")-\r%<[".into());
    check_pattern("\u{0}[@[{<".into());
    check_pattern("a <!A ".into());
    check_pattern("a <? ".into());
    check_pattern("[ (]( ".into());
    check_pattern(Pattern {
        prefix: "".into(),
        repeating_pattern: "`a`".into(),
        postfix: "`".into(),
    });
    check_pattern("\\``".into());
    check_pattern("a***b~~".into());
    check_pattern("*~~\u{a0}".into());
    check_pattern("[*_a".into());
    check_pattern("a***_b__".into());
    check_pattern("a***".into());
    check_pattern("[[]()".into());
    check_pattern("[a](<".into());
    exit_code
}

/// Start fuzzing on given number of threads in parallel.
fn fuzz(num_cpus: usize) {
    let literals = &literals::get();

    let num_batches_finished = AtomicU64::new(0);
    let num_batches_finished = &num_batches_finished;
    let mut rng = Xoshiro256Plus::from_rng(&mut rand::thread_rng()).unwrap();
    let start_time = Instant::now();
    let mut pattern_times = Vec::with_capacity(num_cpus);
    for _ in 0..num_cpus {
        pattern_times.push(Mutex::new((Pattern::default(), Instant::now())));
    }

    // start threads
    thread::scope(|s| {
        // worker threads
        let mut threads: Vec<_> = (0..num_cpus)
            .map(|i| {
                // Get unique RNG for each thread. Read docs of Xoshiro256Plus for further information.
                rng.jump();
                let rng = rng.clone();
                let pattern_time = &pattern_times[i];
                s.spawn(move |_| {
                    worker_thread_fn(literals, &num_batches_finished, rng, pattern_time)
                })
            })
            .collect();

        // timeout thread
        threads.push(s.spawn(|_| {
            let mut prev_batches = 0;

            loop {
                std::thread::sleep(Duration::from_millis(MAX_MILLIS as u64 * 10));
                for pattern_time in &pattern_times {
                    let (pattern, time) = &*pattern_time.lock().unwrap();
                    if time.elapsed().as_millis() > MAX_MILLIS * 10 {
                        // We assume a loop / too long behaviour in pulldown-cmark.
                        // We print the pattern and exec ourselves again to not infinite-loop a thread.
                        // See <https://github.com/raphlinus/pulldown-cmark/pull/286#issuecomment-490315582>
                        // for more information.
                        println!(
                            "Thread timeout triggered (thread took too long) for Pattern: {:?}\n\
                             Restarting process...",
                            pattern,
                        );
                        let args: Vec<_> = env::args().collect();
                        Command::new(&args[0]).args(&args[1..]).exec();
                        unreachable!();
                    }
                }

                // measure and print throughput
                let batches_finished = num_batches_finished.load(Ordering::Relaxed);
                let patterns_finished = batches_finished * BATCH_SIZE as u64;
                let elapsed_secs = start_time.elapsed().as_secs();

                // if for some reason we are super-fast, we must avoid div-by-zero
                if batches_finished != prev_batches && elapsed_secs > 0 && patterns_finished > 0 {
                    println!(
                        "Tested patterns: {}\t\tThroughput: {} patterns / s",
                        patterns_finished,
                        patterns_finished / elapsed_secs,
                    );
                    prev_batches = batches_finished;
                }
            }
        }));

        for thread in threads {
            println!("Joined thread: {:?}", thread.join());
        }
    })
    .unwrap();
}

/// Function executed in worker-threads. Infinite loop generating and testing patterns.
fn worker_thread_fn(
    literals: &Vec<Vec<u8>>,
    num_batches_finished: &AtomicU64,
    mut rng: Xoshiro256Plus,
    pattern_time: &Mutex<(Pattern, Instant)>,
) {
    let pattern_distr = UniformPatterns::new(&literals);
    loop {
        // test those patterns, catching panics of pulldown-cmark (and our code)
        for _ in 0..BATCH_SIZE {
            let pattern = pattern_distr.sample(&mut rng);
            // code-block just to be double-sure that the lock is dropped to not poison it
            {
                *pattern_time.lock().unwrap() = (pattern.clone(), Instant::now());
            }
            let _ = test_catch_unwind(&pattern);
        }

        let _ = num_batches_finished.fetch_add(1, Ordering::Relaxed);
    }
}

fn test_catch_unwind(pattern: &Pattern) -> Result<PatternResult, ()> {
    let res = panic::catch_unwind(|| test(&pattern));
    if res.is_err() {
        println!("Panic caught. Pattern: {:?}", pattern);
    }
    res.map_err(|_| ())
}

/// Test a specific pattern, returning the pattern result.
///
/// This function prints its results to stdout.
/// No further handling is needed, the returned score is for debugging purposes mostly.
fn test(pattern: &Pattern) -> PatternResult {
    let mut time_samples = [(0.0, 0.0); SAMPLE_SIZE];
    let mut res = PatternResult::TooLong;

    // Test a pattern a number of times until its first negative
    // to reduce the number of false positives. This allows us to keep
    // the threshold relatively low since the false positive rate
    // drops exponentially with the number of retests (assuming independence
    // of false positive occurrence).
    for _ in 0..TEST_COUNT {
        res = test_pattern(pattern, &mut time_samples);

        match res {
            PatternResult::Linear(..) => return res,
            PatternResult::NonLinear(_score) => {
                // retest
            }
            PatternResult::TooLong => {
                println!(
                    "\n\
                    possible non-linear behaviour found due to exceeding MAX_MILLIS (parsing took too long)\n\
                    pattern: {:?}\n\
                    score: 0\n\
                    {:?}\n",
                    pattern,
                    time_samples,
                );
                return res;
            }
        };
    }

    if let PatternResult::NonLinear(score) = res {
        println!(
            "\n\
             possible non-linear behaviour found\n\
             pattern: {:?}\n\
             score: {}\n\
             {:?}\n",
            pattern, score, time_samples,
        );
    }

    res
}

#[derive(Debug, Clone, Copy)]
enum PatternResult {
    /// Probably linear growth (score)
    Linear(f64),
    /// Probably non-linear growth (score)
    NonLinear(f64),
    /// Execution took too long, assumed non-linear
    TooLong,
}

impl PatternResult {
    fn score(&self) -> f64 {
        match *self {
            PatternResult::Linear(score) | PatternResult::NonLinear(score) => score,
            PatternResult::TooLong => 0.0,
        }
    }
}

/// Tests a specific pattern, returning measurement and scoring outcomes.
fn test_pattern(pattern: &Pattern, time_samples: &mut [(f64, f64)]) -> PatternResult {
    let mut buf = String::with_capacity(NUM_BYTES);
    let sample_count = time_samples.len();
    let mut i = 0;

    while i < sample_count {
        let n = sample_pattern(pattern, &mut buf, i + 1, sample_count); // FIXME: set actual values
        let dur = time_needed(&buf);
        time_samples[i] = (n as f64, dur.as_nanos() as f64);
        if DEBUG_LEVEL >= 3 {
            println!("duration: {}", dur.as_nanos());
        }

        if dur.as_millis() > MAX_MILLIS {
            return PatternResult::TooLong;
        }
        i += 1;
    }

    let (score, non_linear) = SCORE_FUNCTION(&time_samples);

    if DEBUG_LEVEL >= 1 {
        println!("{:<30}{:?}", score, pattern);
    }
    if DEBUG_LEVEL >= 2 {
        println!("{:?}", time_samples);
    }

    if non_linear {
        PatternResult::NonLinear(score)
    } else {
        PatternResult::Linear(score)
    }
}

/// Returns the number of repeated patterns.
fn sample_pattern(
    pattern: &Pattern,
    buf: &mut String,
    sample_size: usize,
    sample_count: usize,
) -> usize {
    buf.clear();
    buf.push_str(&pattern.prefix);

    let target_byte_count = sample_size * NUM_BYTES / sample_count;
    let target_repeat_bytes = target_byte_count - pattern.prefix.len() - pattern.postfix.len();
    let num_repeats = target_repeat_bytes / pattern.repeating_pattern.len();

    for _ in 0..num_repeats {
        buf.push_str(&pattern.repeating_pattern);
    }

    buf.push_str(&pattern.postfix);
    num_repeats
}

/// Returns the length of the tested substring and the time needed for parsing given string.
fn time_needed(sample: &str) -> Duration {
    // perform actual time measurement
    let clock = Clock::<ThreadCpuTime>::now();
    let parser = Parser::new_ext(sample, Options::all());
    parser.for_each(|evt| {
        black_box::black_box(evt);
    });
    clock.elapsed()
}
