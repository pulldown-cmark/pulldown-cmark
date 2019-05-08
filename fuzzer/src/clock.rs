use std::time::Duration;
use std::mem;
use std::io::Error;
use std::marker::PhantomData;

pub enum Monotonic {}
pub enum ThreadCpuTime {}

pub struct Clock<T> {
    dur: Duration,
    _marker: PhantomData<T>,
}

impl<T: GetTime> Clock<T> {
    pub fn now() -> Clock<T> {
        Clock {
            dur: T::time(),
            _marker: PhantomData,
        }
    }

    pub fn elapsed(&self) -> Duration {
        T::time() - self.dur
    }
}

pub trait GetTime {
    fn time() -> Duration;
}

impl GetTime for Monotonic {
    fn time() -> Duration {
        time(libc::CLOCK_MONOTONIC)
    }
}

impl GetTime for ThreadCpuTime {
    fn time() -> Duration {
        time(libc::CLOCK_THREAD_CPUTIME_ID)
    }
}

fn time(clk_id: libc::clockid_t) -> Duration {
    unsafe {
        let mut timespec = mem::zeroed();
        if libc::clock_gettime(clk_id, &mut timespec) != 0 {
            Err(Error::last_os_error()).unwrap()
        }
        Duration::new(timespec.tv_sec as u64, timespec.tv_nsec as u32)
    }
}
