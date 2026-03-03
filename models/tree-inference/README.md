---
id: tree_inference
title: Tree inference
sidebar_label: Tree inference models
---

# Tree inference models

# Jukes-Cantor

  - **Model files:** `models/tree-inference/tree_inference.tppl`, `models/tree-inference/tree_inference_pruning.tppl`, `models/tree-inference/tree_inference_pruning_scaled.tppl`

  - **Example input data:** `models/tree-inference/data/testdata_tree_inference.json`

  - **Brief info:** The model version `tree_inference.tppl` is a simple backwards tree reconstruction based on aligned DNA sequence data (converted to integers) as input, and using the Jukes-Cantor model of nucleotide substitution. There are also other and more computationally efficient model versions:  `tree_inference_pruning.tppl` using hard-coded pruning (Felsenstein's pruning algorith) and `tree_inference_pruning_scaled.tppl` with pruning and a scaled approach to the messages. Use the scaled version for efficient inference on larger datasets.

# GTR
  - **Model file:** `models/tree-inference/tree_inference_pruning_gtr.tppl`

  - **Example input data:** `models/tree-inference/data/testdata_tree_inference.json`

  - **Brief info:** The basic models using Jukes-Cantor above can easily be extended to adhere other models of nucleotide substitution. As an example, this is the GTR model with pruning.