REBAR=rebar3
ROOT_TEST=_build/test/lib

C_SRC_DIR = $(shell pwd)/c_src
C_SRC_ENV ?= $(C_SRC_DIR)/env.mk

#regenerate all the time the env.mk
ifneq ($(wildcard $(C_SRC_DIR)),)
	GEN_ENV ?= $(shell erl -noshell -s init stop -eval "file:write_file(\"$(C_SRC_ENV)\", \
		io_lib:format( \
			\"ERTS_INCLUDE_DIR ?= ~s/erts-~s/include/~n\" \
			\"ERL_INTERFACE_INCLUDE_DIR ?= ~s~n\" \
			\"ERL_INTERFACE_LIB_DIR ?= ~s~n\", \
			[code:root_dir(), erlang:system_info(version), \
			code:lib_dir(erl_interface, include), \
			code:lib_dir(erl_interface, lib)])), \
		halt().")
    $(GEN_ENV)
endif

include $(C_SRC_ENV)

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

# common tests

ct:
	mkdir -p log
	${REBAR} ct --compile_only
	ct_run  -no_auto_compile \
			-cover test/cover.spec \
			-dir $(ROOT_TEST)/epqueue/test \
			-pa $(ROOT_TEST)/*/ebin \
			-logdir log

# cpplint and cppcheck

cpplint:
	cpplint --counting=detailed \
	        --filter=-legal/copyright,-build/include_subdir,-build/include_order,-whitespace/blank_line,-whitespace/braces,-whitespace/indent,-whitespace/parens,-whitespace/newline \
            --linelength=300 \
			--exclude=c_src/*.o --exclude=c_src/*.mk  \
			c_src/*.*

cppcheck:
	cppcheck -j 8 \
             -I $(ERTS_INCLUDE_DIR) \
             -I $(ERL_INTERFACE_INCLUDE_DIR) \
             --force \
             --enable=all \
	 		 --xml-version=2 \
	 		 --output-file=cppcheck_results.xml \
	 		 c_src/
