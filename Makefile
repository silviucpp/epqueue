REBAR=rebar3

# benchmarks

BENCH_PROFILE_ARGS=-pa _build/bench/lib/epqueue/benchmarks -pa _build/bench/lib/*/ebin -noshell
ELEMENTS=1000000
MAX_PRIORITY=1000000
USE_LOCK=false
PROCS=1

setup_benchmark:
	${REBAR} as bench compile

bench_serial: setup_benchmark
	erl $(BENCH_PROFILE_ARGS) -eval "benchmark:benchmark_serial($(ELEMENTS), $(MAX_PRIORITY), $(USE_LOCK))" -eval "init:stop()."

bench_concurrent: setup_benchmark
	erl $(BENCH_PROFILE_ARGS) -eval "benchmark:benchmark_concurrent($(PROCS), $(ELEMENTS), $(MAX_PRIORITY))" -eval "init:stop()."
