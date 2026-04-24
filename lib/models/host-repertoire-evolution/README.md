---
id: host_repertoire_evolution
title: Host-repertoire model
sidebar_label: Host-repertoire model
---

# Host repertoire evolution

  - **Model files:** `models/host-repertoire-evolution/flat-root-prior-HRM.tppl` and `models/host-repertoire-evolution/subroot-HRM.tppl`.

  - **Example input data:** `models/host-repertoire-evolution/testdata_flat-root-prior-HRM.json` and `models/host-repertoire-evolution/testdata_subroot-HRM.json`.

  - **Brief info:** The `subroot` model considers that there is a branch subtending the root of the symbiont tree, which allows the host gain/loss process to reach stationarity before diversification of the symbiont clade starts. This version of the model is identical to the model implemented in RevBayes. The `flat-root-prior` model considers that the symbiont tree - and thus the host gain/loss process - starts at the root.

  - **Reference:**
  Braga, M. P., Landis, M. J., Nylin, S., Janz, N., & Ronquist, F. (2020). Bayesian inference of ancestral host–parasite interactions under a phylogenetic model of host repertoire evolution. Systematic biology, 69(6), 1149-1162. https://doi.org/10.1093/sysbio/syaa019
