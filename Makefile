# ANSI escape sequence for red text
RED=\033[0;31m
# ANSI escape sequence for resetting text color
RESET=\033[0m

tppl_name=tpplc
bin_path=${HOME}/.local/bin
src_path=${HOME}/.local/src/treeppl/
tppl_src=src/.

tppl_tmp_file := $(shell mktemp)
build/${tppl_name}: $(shell find . -name "*.mc" -o -name "*.syn")
	mi syn src/treeppl.syn src/treeppl-ast.mc
	time mi compile src/${tppl_name}.mc --output ${tppl_tmp_file}
	mkdir -p build
	cp ${tppl_tmp_file} build/${tppl_name}
	rm ${tppl_tmp_file}

install: build/${tppl_name}
	echo "Make sure you extend the MCORE_LIBS variable with"
	cp build/${tppl_name} ${bin_path}/${tppl_name}
	chmod +x ${bin_path}/${tppl_name}
	cp -rf $(tppl_src) $(src_path)
	@echo "\n${RED}Attention:${RESET}"
	@echo "${tppl_name} has been installed to ${bin_path} and the CorePPL sources have been installed to $(src_path)."
	@echo "Please, ensure that the PATH and the MCORE_LIBS environment variables have been set accordingly."
	@echo "E.g. under Bash:"
	@echo 'export PATH=$$PATH:'"${bin_path}"
	@echo 'export MCORE_LIBS=$$MCORE_LIBS:treeppl='"$(src_path)\n"

uninstall:
	rm ${bin_path}/${tppl_name}
	rm -rf $(src_path)

test: src/treeppl-to-coreppl/compile.mc
	mi run src/treeppl-to-coreppl/compile.mc --test

clean:
	rm src/treeppl-ast.mc
	rm build/*
	