use ndarray::{ArrayViewMut1, Array2};
use ndarray_stats::CorrelationExt;
use itertools::Itertools;
use crate::SAMPLE_SIZE;

#[allow(dead_code)]
pub fn pearson_correlation(array: &[(f64, f64)]) -> (f64, bool) {
    let mut vec = Vec::with_capacity(array.len());
    vec.extend(array.iter().cloned().map(|(x, y)| [x, y]));
    let array = Array2::from(vec);
    let array = array.t();
    let corr = array.pearson_correlation()[[1, 0]];
    (corr, corr < super::ACCEPTANCE_CORRELATION)
}

#[allow(dead_code)]
pub fn slope_stddev(array: &[(f64, f64)]) -> (f64, bool) {
    let mut slopes = [0f64; SAMPLE_SIZE*2 * (SAMPLE_SIZE*2 - 1) / 2];
    let mut i = 0;

    for (point1, point2) in (0..array.len()).tuple_combinations() {
        let (x1, y1) = array[point1];
        let (x2, y2) = array[point2];
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
    let mut array = ArrayViewMut1::from(slopes);
    let mean = array.sum() / array.len() as f64;
    array.mapv_inplace(|slope| (slope - mean).powi(2));
    let sum = array.sum();
    let in_sqrt = sum / array.len() as f64;
    let stddev = in_sqrt.sqrt();

    (stddev, stddev > super::ACCEPTANCE_STDDEV)
}
