# ANSI escape sequence for red text
RED=\033[0;31m
# ANSI escape sequence for resetting text color
RESET=\033[0m

tppl_name=tpplc
bin_path=${HOME}/.local/bin
src_path=${HOME}/.local/src/treeppl/
tppl_src=src/.
tppl_models=models

build/${tppl_name}: src/treeppl-ast.mc $(shell find src -path 'src/lib' -prune -o \( -name "*.mc" -o -name "*.syn" \) -print)
	mkdir -p build
	time mi compile src/tpplc.mc --output $@

src/treeppl-ast.mc: src/treeppl.syn
	mi syn $< $@

install: build/${tppl_name}
	echo "Make sure you extend the MCORE_LIBS variable with"
	cp build/${tppl_name} ${bin_path}/${tppl_name}
	chmod +x ${bin_path}/${tppl_name}
	cp -rf $(tppl_src) $(src_path)
	cp -rf $(tppl_models) $(src_path)
	@echo "\n${RED}Attention:${RESET}"
	@echo "${tppl_name} has been installed to ${bin_path} and the TreePPL sources have been installed to $(src_path)."
	@echo "Please, ensure that the PATH and the MCORE_LIBS environment variables have been set accordingly."
	@echo "E.g. under Bash:"
	@echo 'export PATH=$$PATH:'"${bin_path}"
	@echo 'export MCORE_LIBS=$$MCORE_LIBS:treeppl='"$(src_path)\n"

uninstall:
	rm ${bin_path}/${tppl_name}
	rm -rf $(src_path)

.PHONY: test-standard
test-standard:
	mi compile src/lib/standard.mc --test --output build/standard
	build/standard

.PHONY: test-compile
test-compile:
	mi compile src/treeppl-to-coreppl/compile.mc --test --output build/compile
	build/compile

# Configurations to run tests with, as flags with spaces replaced with
# "_", because we want a list of lists here, and that's not actually
# available. It's possible we should also include variations on the
# resample flag later, but it's already a large set of configurations
# to test. The methods that consider the resample flag are marked with
# "resample" below.
TEST_CONFIGURATIONS := -m_is-lw_--cps_none
TEST_CONFIGURATIONS += -m_is-lw_--cps_partial
TEST_CONFIGURATIONS += -m_is-lw_--cps_full
TEST_CONFIGURATIONS += -m_smc-bpf_--cps_full  #resample
TEST_CONFIGURATIONS += -m_smc-bpf_--cps_partial  #resample
TEST_CONFIGURATIONS += -m_smc-apf_--cps_full  #resample
TEST_CONFIGURATIONS += -m_smc-apf_--cps_partial  #resample
TEST_CONFIGURATIONS += -m_mcmc-lightweight_--cps_none #resample
TEST_CONFIGURATIONS += -m_mcmc-lightweight_--align_--cps_none #resample
TEST_CONFIGURATIONS += -m_mcmc-lightweight_--align_--cps_full #resample
TEST_CONFIGURATIONS += -m_mcmc-lightweight_--align_--cps_partial #resample
TEST_CONFIGURATIONS += -m_mcmc-trace_--cps_none
TEST_CONFIGURATIONS += -m_mcmc-naive_--cps_none
TEST_CONFIGURATIONS += -m_pmcmc-pimh_--cps_full
TEST_CONFIGURATIONS += -m_pmcmc-pimh_--cps_partial

MODELS := $(shell find models -name "*.tppl")
MODEL_CONFIGS := $(foreach model,$(MODELS),$(foreach config,$(TEST_CONFIGURATIONS),$(model)@$(config)))

.PHONY: $(MODEL_CONFIGS)
$(MODEL_CONFIGS):
	@set -ue; \
	path=$(firstword $(subst @, ,$@)); \
	conf="$(subst _, ,$(lastword $(subst @, ,$@)))"; \
	mkdir -p build/$$(dirname $$path); \
	build/${tppl_name} $$conf $$path -p 2 --debug-phases --output build/$@ > build/$@.c.out 2> build/$@.c.err || { st=$$?; echo "$$path,$$conf,compile failure,$$st"; exit $$st; }; \
	build/$@ "$${path%.tppl}.json" > build/$@.r.out 2> build/$@.r.err || { st=$$?; echo "$$path,$$conf,run failure,$$st"; exit $$st; }; \
	echo "$$path,$$conf,success,0"

.PHONY: test-models
test-models: $(MODEL_CONFIGS)

.PHONY: print-model-targets
print-model-targets:
	@echo $(MODEL_CONFIGS)

.PHONY: print-models
print-models:
	@echo $(MODELS)

.PHONY: test
test: test-compile test-standard test-models

clean:
	rm -f src/treeppl-ast.mc
	rm -rf build/*
	rm -f out compile
