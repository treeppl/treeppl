---
id: metabarcoding
title: Abundances from metabarcoding
sidebar_label: Abundances from metabarcoding
sidebar_position: 1
---

# Abundances from metabarcoding

# Basic model

  - **Model file:** `models/metabarcoding/optmodel.tppl`

  - **Example input data:** `models/metabarcoding/data/testdata_optmodel.json`

  - **Brief info:** The example model `optmodel.tppl` is a probabilistic model where the parameter values are inferred from the read numbers of spike-in species in metabarcoding samples. The resulting distributions can be used to predict abundances of other insect species in the sample set. The provided test data with spike-in reads and prior settings comes from the below publication. 

  - **Reference:**
  Iwaszkiewicz-Eggebrecht, E., Granqvist, E., Buczek, M., Prus, M., Kudlicka, J., Roslin, T., Tack, A. J. M., Andersson, A. F., Miraldo, A., Ronquist, F., & Łukasik, P. (2023). Optimizing insect metabarcoding using replicated mock communities. Methods in Ecology and Evolution, 14, 1130–1146. https://doi.org/10.1111/2041-210X.14073