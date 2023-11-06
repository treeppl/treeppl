# TreePPL – A Universal Probabilistic Programming Language for Evolutionary Biology

## Installation

TreePPL works on Linux (running both on bare metal, as as under WSL) and on Mac.

Tested on

- Windows 10, WSL2
- Ubuntu 22.04.1 LTS
- M1 and M2 Macs

**Windows pre-prerequisites**

Install WSL2 and GUI support https://learn.microsoft.com/en-us/windows/wsl/tutorials/gui-apps.

Then follow the Linux instructions.

**Linux prerequisites**

```
sudo apt update
sudo apt upgrade
sudo apt install unzip
sudo apt install bubblewrap
sudo apt install bzip2
sudo apt install python3-pip
```

If additional packages are missing during the subsequent installation stages, install them as well, please.

**Mac prerequistes**

Updated MacOS, command line tools, brew, etc.

In particular:

```
sudo rm -rf /Library/Developer/CommandLineTools
xcode-select --install
```

**Opam and OCaml on Linux** 

We need the OCaml 5 programming language and the Opam package manager to compile Miking, which is used to compile TreePPL.

You can save the following to a script, make it executable and run it.

```
#!/usr/bin/env bash
# Tested on Ubuntu 20.04 and 22.04.1 LTS WSL

####################
## Opam and OCaml ##
####################
read -p "Install/reinstall Opam and OCaml? " -n 1 -r; echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
  ## Clean up
  rm -rf ~/.opam

  ## Install opam
  sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)

  ## Install required packages
  sudo apt install m4 make gcc

  ## Install Ocaml 5 and standard packages
  opam init -y
  opam update
  opam switch create 5.0.0
  eval $(opam env)
  opam install -y dune linenoise utop ocp-indent merlin

  ## Install packages required in Miking
  opam pin ocamlformat 0.24.1
  opam install -y pyml toml owl
fi
```

**Opam and OCaml on Mac**

Follow the [workshop notes](https://miking.org/workshop-2022-install) for Apple Silicon, but you can use OCaml 5.0.0 and you don't need `sundials`.

**Miking and TreePPL with bash (default shell on Linux)**

```
eval $(opam env)
gh repo clone miking-lang/miking
cd miking
make install
cd ..
gh repo clone miking-lang/miking-dppl
cd miking-dppl
export PATH="$HOME/.local/bin:$PATH"
make install
export MCORE_LIBS=coreppl=$HOME/.local/src/coreppl/
cd ..
gh repo clone https://github.com/treeppl/treeppl.git
cd treeppl
export MCORE_LIBS=$MCORE_LIBS:treeppl=$HOME/.local/src/treeppl/
make install
cd ..
pip install "git+https://github.com/treeppl/treeppl-python#egg=treeppl"
```

You can put the environment stuff in your `.bashrc`. Fire up `gnome-text-editor ~/.bashrc` (or another text editor) and add

```
## Customizations for TreePPL
eval $(opam env)
export PATH="$HOME/.local/bin:$PATH"
export MCORE_LIBS="coreppl=$HOME/.local/src/coreppl/"
export MCORE_LIBS="$MCORE_LIBS:treeppl=$HOME/.local/src/treeppl/"
```

**Miking and TreePPL with zsh (default for Mac)**

If you use `zsh`, which is default on the Mac, or other shells such as `fish`, please make sure you that you use the correct syntax for setting environment variables, as it is slightly different.

**Running an example**

```
pip install matplotlib
pip install seaborn
sudo apt-get install python3-tk

gh repo clone treeppl/treeppl-python
cd treeppl-python/examples
python3 coin.py
```

**Setup up VSCode – Windows specific**

Install WSL Extension and/or Remote-extension-pack

https://code.visualstudio.com/docs/remote/wsl
https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.vscode-remote-extensionpack

from a directory that you want to develop from and do `code .`

## About TreePPL

### TL;DR

TreePPL is a universal probabilistic programming language (PPL) with immutable semantics.  We aim TreePPL primarily at computational biologists and bioinformaticians, however due to its universality, empiricists from all domains are welcome to experiment with the language and join the effort.  It is comparable to STAN, Pyro, etc.; however, it has some expressivity and inference features that make it stand out, see our [release notes](https://doi.org/10.1101/2023.10.10.561673).

### Design goals

The ultimate vision of probabilistic programming is to provide expressive model description languages, while at the same time supporting the automated generation of efficient inference algorithms. Thisallows empiricists to easily and succinctly describe any model they might be interested in, relying on the automated machinery to provide efficient inference algorithms for that model.

Current probabilistic programming languages (PPLs) are often difficult to use for empiricists. Furthermore, even though there is now swift progress in PPL inference strategies, there is still a substantial gap in many domains before PPL systems can compete successfully with dedicated software, or even provide computationally feasible solutions.

The aim of the TreePPL project is to develop a universal probabilistic programming language, inspired by use-cases in statistical phylogenetics. There are several specific design goals:

1. TreePPL should be easy to use for empiricist. A source of inspiration in this context is WebPPL, which we think is one of the most accessible PPLs in terms of syntax. Beyond an intuitive syntax, TreePPL also needs to have extensive support for model components that are commonly used in phylogenetics.
2. TreePPL should provide state-of-the-art efficiency in the inference algorithms it generates from phylogenetic model descriptions. TreePPL should support advanced users that want to experiment with inference algorithms or develop entirely new inference strategies for phylogenetic models.
3. TreePPL should provide a number of pre-implemented models that users can use as starting points.
4. Phylogenetic data should be easy to handle in TreePPL.

## References and citation

Please cite TreePPL the [release notes](https://doi.org/10.1101/2023.10.10.561673):

```
TreePPL: A Universal Probabilistic Programming Language for Phylogenetics
Viktor Senderov, Jan Kudlicka, Daniel Lundén, Viktor Palmkvist, Mariana P. Braga, Emma Granqvist, David Broman, Fredrik Ronquist
bioRxiv 2023.10.10.561673; doi: https://doi.org/10.1101/2023.10.10.561673
```

Further reference, [concept paper](https://doi.org/10.1038/s42003-021-01753-7), here we show the feasibility of PPL's for phylogenetics:

```
Ronquist, F., Kudlicka, J., Senderov, V. et al. Universal probabilistic programming offers a powerful approach to statistical phylogenetics. Commun Biol 4, 244 (2021). https://doi.org/10.1038/s42003-021-01753-7
TreePPL will be built on top of Miking, a language framework for constructing efficient compilers for domain-specific languages.
```

This web site will be continuously updated, with the ultimate aim of developing it into the primary online resource for the TreePPL community.
