//! Functions detecting non-linear growth.

use crate::SAMPLE_SIZE;
use itertools::Itertools;
use ndarray::{Array2, ArrayViewMut1};
use ndarray_stats::CorrelationExt;

/// Calculates the pearson_correlation of the passed `(len, time)`-array.
#[allow(dead_code)]
pub fn pearson_correlation(time_samples: &[(f64, f64)]) -> (f64, bool) {
    let mut vec = Vec::with_capacity(time_samples.len());
    vec.extend(time_samples.iter().cloned().map(|(x, y)| [x, y]));
    let time_samples = Array2::from(vec);
    let time_samples = time_samples.t();
    let corr = time_samples.pearson_correlation()[[1, 0]];
    (corr, corr < super::ACCEPTANCE_CORRELATION)
}

/// Calculates the stdandard deviation of the slopes between all combinations of points.
///
/// We interpret the `(len, time)`-tuples of the array as `(x, y)`-coordinates / points on a graph.
/// Each tuple in the array is one point / coordinate.
/// If the time increase is perfectly linear, the slopes of the line running through each two
/// adjacent points are all equal.
/// In fact the slope between any two points from the array will be the same.
/// If all slopes are the same, the standard deviation of all of those slopes will be zero.
///
/// However, in the real world there are inconsistencies, deviations, rounding and outliers.
/// As such, the points won't be on a perfect line.
/// Nevertheless, if it's linear, the slopes will be close enough to each other, that small outliers
/// won't increase the standard deviation too much.
/// The more points we add and the more slopes we add, the more resilient to outliers the result
/// will become. That's also why we don't only take into account the slopes between neighboring
/// points (`A-B`, `B-C`, `C-D`), but the slopes between any two points (`A-B`, `A-C`, `A-D`,
/// `B-C`, `B-D`, `C-D`).
///
/// If the function is not linear, the slopes will all be different, resulting in a higher stddev.
#[allow(dead_code)]
pub fn slope_stddev(time_samples: &[(f64, f64)]) -> (f64, bool) {
    let mut slopes = [0f64; SAMPLE_SIZE * 2 * (SAMPLE_SIZE * 2 - 1) / 2];
    let mut i = 0;

    let mean_time: f64 =
        time_samples.iter().map(|tup| tup.1).sum::<f64>() / time_samples.len() as f64;

    for (point1, point2) in (0..time_samples.len()).tuple_combinations() {
        let (x1, y1) = time_samples[point1];
        let (x2, y2) = time_samples[point2];
        let dx = x2 - x1;
        let dy = y2 - y1;
        let slope = dy / dx / mean_time;
        slopes[i] = slope;
        i += 1;
    }

    let slopes = &mut slopes[..i];

    if super::DEBUG_LEVEL >= 2 {
        println!("slopes: {:?}", slopes);
    }

    // calculate standard deviation
    let mut time_samples = ArrayViewMut1::from(slopes);
    let mean = time_samples.sum() / time_samples.len() as f64;
    time_samples.mapv_inplace(|slope| (slope - mean).powi(2));
    let sum = time_samples.sum();
    let in_sqrt = sum / time_samples.len() as f64;
    let stddev = in_sqrt.sqrt();

    (stddev, stddev > super::ACCEPTANCE_STDDEV)
}
