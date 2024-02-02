# Fuzzer for detecting superlinear growth in pulldown-cmark

This fuzzer tries to find superlinear growth in pulldown-cmark wrt. input length.
The general approach is to parse the source code of pulldown-cmark, extract
literals which are used in branching code (if-conditions, match patterns,
match guards, …) and add some manually.
Random combinations of those literals are generated.
The pulldown-cmark parser is then timed against repetitions of different length
of those literals to identify superlinear parsing behaviour.

## Running

Running the fuzzer can be done by executing the `./run` script.
The constants in `main.rs` should be tweaked for the system the fuzzer is
executed on, as some of them are system dependent.
The number of threads can be changed in the `main()` function.
It defaults to using 80% of the number of system threads, rounding up.

The fuzzer will run until manually stopped.
All output will be written to the file `output` and will most likely contain
many false positives.
Therefore, after fuzzing has been stopped, the `./retest-output` script should
be executed.
It'll retest found patterns several times to remove as many false positives as
possible (but will most likely also remove some true positives, which shouldn't
be a problem because most true positives are very redundant).
Once finished, it'll output the total number of panics, thread timeouts, joined
threads, UTF8 errors, false positives and true positives.

* Panicks are panicks encountered while testing a pattern and are most likely
occuring within pulldown-cmark.
* Thread timeouts mean that a thread took longer than `10 * MAX_MILLIS` for a
single pattern.
  It could indicate an infinite loop within pulldown-cmark or the fuzzer logic.
* Joined threads are always bugs within the fuzzer and should be reported.
  Every joined thread will reduce the throughput of patterns, as joined threads
  are removed from the thread pool.
* UTF8 errors occur when a pattern (combination of literals) isn't valid UTF8.
  This shouldn't happen and should always be reported.

All four of the above error types should be inspected manually.
However, the `./retest-output` script already groups panicks by calling the
`group-panics` python script.
That script retests panicks and groups them by their panic message and backtrace.
For every panic message / backtrace, it creates minimal reproduction cases
for every pattern resulting in that panic, such that they can be easily pasted
into github issues.

**Script `plot`**:

For easy rendering of the printed timing samples by the fuzzer, the `plot`
script can take the printed samples-array as argument and will render
those (input-length, measured-time)-pairs on a graph
(wolframalpha only support a limited number of ponits on a graph).
