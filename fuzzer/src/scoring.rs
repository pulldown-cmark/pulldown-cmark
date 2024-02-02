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

    for (point1, point2) in (0..time_samples.len()).tuple_combinations() {
        let (x1, y1) = time_samples[point1];
        let (x2, y2) = time_samples[point2];
        let dx = x2 - x1;
        let dy = y2 - y1;
        let slope = dy / dx;
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

#[cfg(test)]
mod test {
    use super::slope_stddev;

    #[test]
    fn stddev_negative_on_linear() {
        let vec = vec![
            (10.0, 100.0),
            (20.0, 201.0),
            (30.0, 299.5),
            (40.0, 385.0),
            (50.0, 510.0),
        ];
        let test_result = slope_stddev(&vec);

        println!("Score = {}", test_result.0);
        assert!(test_result.1 == false);
    }

    #[test]
    fn stddev_negative_on_noisy_linear() {
        let vec = vec![
            (0.1, 85.0),
            (0.2, 222.0),
            (0.3, 270.5),
            (0.4, 385.0),
            (0.5, 520.0),
        ];
        let test_result = slope_stddev(&vec);

        println!("Score = {}", test_result.0);
        assert!(test_result.1 == false);
    }

    #[test]
    fn stddev_positive_on_quadratic() {
        let vec = vec![
            (0.1, 100.0),
            (0.2, 400.0),
            (0.3, 880.0),
            (0.4, 1630.0),
            (0.5, 2440.0),
        ];
        let test_result = slope_stddev(&vec);

        println!("Score = {}", test_result.0);
        assert!(test_result.1 == true);
    }

    #[test]
    // power of 1.5
    fn stddev_positive_on_semiquadratic() {
        let vec = vec![
            (0.1, 105.0),
            (0.2, 260.0),
            (0.3, 505.0),
            (0.4, 775.0),
            (0.5, 1118.0),
        ];
        let test_result = slope_stddev(&vec);

        println!("Score = {}", test_result.0);
        assert!(test_result.1 == true);
    }
}
