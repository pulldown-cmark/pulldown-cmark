use std::fs;
use std::path::Path;
use std::collections::HashSet;
use std::time::{Duration, Instant};
use std::panic;
use std::sync::mpsc;

use walkdir::WalkDir;
use itertools::Itertools;
use pulldown_cmark::{Parser, Options};
use ndarray::Array2;
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
const SCORE_FUNCTION: fn(&Array2<f64>) -> (f64, bool) = scoring::slope_stddev;
/// If slope_stddev is used, if the standard deviation is larger than this, it's assumed to be non-linear.
const ACCEPTANCE_STDDEV: f64 = 20.0;
/// If pearson_correlation is used, if the correlation coefficient is below this value,
/// it's assumed to be non-linear.
const ACCEPTANCE_CORRELATION: f64 = 0.995;
/// 0 / 1 / 2 / 3
const DEBUG_LEVEL: u8 = 0;

fn main() {
    let num_cpus = num_cpus::get() * 3 / 4;

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
        if count > 0 {
            println!("{:<20} throughput: {}", count, count as u64 / start_time.elapsed().as_secs());
        }
    }
}

fn test(pattern: &str) {
    let (_, _, non_linear) = test_pattern(pattern, SAMPLE_SIZE, false);
    if non_linear {
        let (array, score, non_linear) = test_pattern(pattern, SAMPLE_SIZE*2, true);
        if non_linear {
            print_find(pattern, &array, score);
        }
    }
}

fn test_pattern(pattern: &str, sample_size: usize, remove_outliers: bool) -> (Array2<f64>, f64, bool) {
    let s = pattern.repeat(NUM_BYTES / pattern.len());
    let mut array = Array2::zeros((2, sample_size));

    let mut i = 0;
    while i < sample_size {
        let (dur, n) = calculate_point(&s, i+1, sample_size);
        array[[0, i]] = n as f64;
        array[[1, i]] = dur.as_nanos() as f64;

        if remove_outliers && i > 0 && array[[1, i-1]] > array[[1, i]] {
            // We have an outlier, possibly due to rescheduling.
            // Redo from the last sample
            i -= 1;
            continue;
        }

        if dur.as_millis() > MAX_MILLIS {
            return (array, 0.0, true);
        }
        i += 1;
    }

    let (score, non_linear) = SCORE_FUNCTION(&array);

    if DEBUG_LEVEL >= 1 {
        println!("{:<30}{}", score, pattern);
    }
    if DEBUG_LEVEL >= 2 {
        println!("{}", array.t());
    }

    return (array, score, non_linear)
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

fn print_find(pattern: &str, array: &Array2<f64>, stddev: f64) {
    println!("
possible non-linear behaviour found
pattern: {:?}
{}
score: {}
\
    ", pattern, array.t(), stddev);
}

