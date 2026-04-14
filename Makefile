# ANSI escape sequence for red text
RED=\033[0;31m
# ANSI escape sequence for resetting text color
RESET=\033[0m

tppl_name=tpplc
bin_path=${HOME}/.local/bin
src_path=${HOME}/.local/src/treeppl/
tppl_src=src/.
tppl_models=models

mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
current_dir := $(dir $(mkfile_path))
ifdef MCORE_LIBS
MCORE_LIBS:=treeppl=$(current_dir)/src:$(MCORE_LIBS)
else
MCORE_LIBS:=treeppl=$(current_dir)/src
endif
export MCORE_LIBS

build/${tppl_name}: build/src/treeppl-ast.mc $(shell find src -path 'src/lib' -prune -o \( -name "*.mc" -o -name "*.syn" \) -print)
	cp build/src/treeppl-ast.mc src/treeppl-ast.mc; mi compile src/tpplc.mc --output $@; e=$$?; rm src/treeppl-ast.mc; exit $e

build/src/treeppl-ast.mc: src/treeppl.syn
	mkdir -p `dirname $@`
	mi syn $< $@

install: build/${tppl_name}
	echo "Make sure you extend the MCORE_LIBS variable with"
	mkdir -p ${bin_path} `dirname ${src_path}`
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

misc/test: misc/test-spec.mc
	mi compile $< --output $@

.PHONY: test
test: misc/test
	+misc/test

clean:
	rm -f src/treeppl-ast.mc
	rm -rf build/*
	rm -f out compile
