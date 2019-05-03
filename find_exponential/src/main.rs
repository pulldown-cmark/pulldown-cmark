use std::fs;
use std::path::Path;
use std::collections::HashSet;
use std::time::{Duration, Instant};
use std::panic;
use std::sync::mpsc;
use std::env;
use std::io::{self, BufRead};

use walkdir::WalkDir;
use itertools::Itertools;
use pulldown_cmark::{Parser, Options};
use rand::seq::SliceRandom;
use threadpool::ThreadPool;

mod literals;
mod black_box;
mod scoring;

/// How many combinations are batch-processed.
const BATCH_SIZE: usize = 10_000;
/// Combination length.
const COMBINATIONS: usize = 4;
/// If parsing takes longer than this many milliseconds, it is assumed to be non-linear.
const MAX_MILLIS: u128 = 500;
/// Size of repetition in bytes to parse.
const NUM_BYTES: usize = 256*1024;
/// Number of samples per pattern tested.
const SAMPLE_SIZE: usize = 5;
/// Score function to use when figuring out if something is non-linear.
/// `scoring::{slope_stddev,pearson_correlation}`
const SCORE_FUNCTION: fn(&Vec<[f64; 2]>) -> (f64, bool) = scoring::slope_stddev;
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
    if let Some("--retest") = arg.as_ref().map(|s| s.as_str()) {
        for pattern in io::stdin().lock().lines() {
            let pattern = pattern.unwrap();
            println!("retesting {:?}", pattern);
            let score = test(&pattern);
            println!("score: {}", score);
        }
        return;
    } else {
        fuzz(num_cpus);
    }
}

fn fuzz(num_cpus: usize) {
    // get all literals from pulldown-cmark's source code
    let walkdir = WalkDir::new("../src")
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .filter(|e| if let Some(ext) = e.path().extension() { ext == "rs" } else { false });

    let mut literals = HashSet::new();
    for file in walkdir {
        if file.path() == Path::new("../src/entities.rs") {
            continue;
        }
        println!("parsing {}", file.path().display());
        let content = fs::read_to_string(file.path())
            .expect(&format!("unable to read file {:?}", file.path()));
        let parsed = syn::parse_file(&content)
            .expect(&format!("unable to parse file {:?}", file.path()));
        literals::extract_literals_from_items(&mut literals, parsed.items);
    }

    let literals: Vec<_> = literals.into_iter()
        .filter(|lit| !lit.contains(&b'\n'))
        .filter(|lit| !lit.is_empty())
        .collect();

    let pool = ThreadPool::new(num_cpus);
    let mut count = -(num_cpus as isize) * BATCH_SIZE as isize;
    let start_time = Instant::now();
    let (tx, rx) = mpsc::channel();
    for _ in 0..num_cpus {
        tx.send(()).unwrap();
    }
    loop {
        rx.recv().unwrap();
        let mut literals = literals.clone();
        let tx = tx.clone();
        count += BATCH_SIZE as isize;
        pool.execute(move || {
            literals.shuffle(&mut rand::thread_rng());
            let combs = literals.iter()
                .combinations(COMBINATIONS)
                .take(BATCH_SIZE);
            for comb in combs {
                let res = panic::catch_unwind(|| {
                    let concatenated = comb.into_iter().flatten().cloned().collect::<Vec<_>>();
                    let s = String::from_utf8(concatenated).unwrap();
                    test(&s);
                });
                if res.is_err() {
                    ::std::process::exit(1);
                }
            }
            tx.send(()).unwrap();
        });
        if count > 0 && start_time.elapsed().as_secs() > 0 {
            println!("{:<20} throughput: {}", count, count as u64 / start_time.elapsed().as_secs());
        }
    }
}

fn test(pattern: &str) -> f64 {
    loop {
        let res1 = test_pattern(pattern, SAMPLE_SIZE, false);
        if let PatternResult::Linear(_array, score) = res1 {
            return score;
        }
        let res2 = test_pattern(pattern, SAMPLE_SIZE*2, true);
        if let PatternResult::Linear(_array, score) = res2 {
            return score;
        }

        // possible non-linear behaviour found

        let score = match res2 {
            PatternResult::Linear(..) => unreachable!(),
            PatternResult::NonLinear(array, score) => {
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
            PatternResult::TooLong(array) => {
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
            PatternResult::HugeOutlier(array, trim_len) => {
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
    /// Probably linear growth (points, score)
    Linear(Vec<[f64; 2]>, f64),
    /// Probably non-linear growth (points, score)
    NonLinear(Vec<[f64; 2]>, f64),
    /// Execution took too long, assumed non-linear
    TooLong(Vec<[f64; 2]>),
    /// Very slow outlier, probably due to different parsing in pulldown-cmark.
    /// (points, number of bytes truncated from end reaching this behaviour)
    ///
    /// Possibly indicates weird behaviour in pulldown-cmark.
    /// See <https://github.com/raphlinus/pulldown-cmark/issues/287> for an example.
    HugeOutlier(Vec<[f64; 2]>, usize),
}

fn test_pattern(pattern: &str, sample_size: usize, recalculate_outliers: bool) -> PatternResult {
    let s = pattern.repeat(NUM_BYTES / pattern.len());
    let mut array = Vec::with_capacity(sample_size);

    let mut i = 0;
    let mut huge_outliers = 0;
    while i < sample_size {
        let (dur, n) = calculate_point(&s, i+1, sample_size);
        array.push([n as f64, dur.as_nanos() as f64]);
        if DEBUG_LEVEL >= 3 {
            println!("duration: {}", dur.as_nanos());
        }

        if i > 0 && array[i-1][1] > array[i][1] * 50.0 {
            huge_outliers += 1;
            if huge_outliers > 10 {
                let last_repetition_start = pattern.len() + s.rfind(pattern).unwrap();
                let trim_len = pattern.len() - (s.len() - last_repetition_start);
                return PatternResult::HugeOutlier(array, trim_len);
            }
        }

        if recalculate_outliers && i > 0 && array[i-1][1] > array[i][1] {
            // We have an outlier, possibly due to rescheduling.
            // Redo from the last sample
            if DEBUG_LEVEL >= 3 {
                println!("removed outlier");
            }
            array.pop();
            array.pop();
            i -= 1;
            continue;
        }

        if dur.as_millis() > MAX_MILLIS {
            return PatternResult::TooLong(array);
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
        PatternResult::NonLinear(array, score)
    } else {
        PatternResult::Linear(array, score)
    }
}

fn calculate_point(s: &str, i: usize, sample_size: usize) -> (Duration, usize) {
    let mut n = s.len() / sample_size * i;
    // find closest byte boundary
    while !s.is_char_boundary(n) {
        n += 1;
    }
    (time_needed(&s[..n]), n)
}

fn time_needed(s: &str) -> Duration {
    let parser = Parser::new_ext(&s, Options::all());
    let time = Instant::now();
    parser.for_each(|evt| {
        black_box::black_box(evt);
    });
    time.elapsed()
}


