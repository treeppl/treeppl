---
id: diversification
title: Diversification models
sidebar_label: Diversification models
---
# Diversification models

# Constant rate birth-death

  - **Model file:** `models/diversification/crbd.tppl`

  - **Example input data:** `models/diversification/data/testdata_crbd.json`

  - **Brief info:** The model simulates the constant rate birth-death process over a phylogenetic tree. The speciation rate is returned.

# ClaDS

  - **Model file:** `models/diversification/clads.tppl`

  - **Example input data:** `models/diversification/data/testdata_clads.json`

  - **Brief info:** The model simulates the ClaDS process over a phylogenetic tree. This implementation of the ClaDS model is a simple version that returns the speciation rate λ; however, it does not return the branch-specific rates.

  - **Reference:**
    Maliet, O., Hartig, F. & Morlon, H. A model with many small shifts for estimating species-specific diversification rates. Nat Ecol Evol 3, 1086–1092 (2019). 
    https://doi.org/10.1038/s41559-019-0908-0