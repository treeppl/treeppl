TESTS=impossible-redraws beta-bernoulli skewed-geometric

# Compiler to test, override with e.g. `make TPPLC=../build/tpplc`
TPPLC ?= tpplc

.PHONY: test $(TESTS) clean

test: $(TESTS)

$(TESTS):
	uv run --locked --project . scripts/run_test.py models/$@ --compiler-path $(TPPLC)

clean:
	rm -rf $(addprefix models/,$(addsuffix /build,$(TESTS)))
