# TreePPL – A Universal Probabilistic Programming Language for Evolutionary Biology

## Installation

TreePPL works on Linux (running both on bare metal, as as under WSL) and on Mac.

Tested on

- Windows 10, WSL2
- Ubuntu 22.04.1 LTS
- M1 and M2 Macs

For installation guidelines, go to TreePPL [website](https://treeppl.org/docs/Howtos#installation-guides)

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
