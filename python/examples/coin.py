#!/usr/bin/env python3

import sys
sys.path.append("..")

import treeppl
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

source = """\
model function coin(outcomes: Bool[]): Real {
  assume p ~ Beta(2.0, 2.0);
  for i in 1 to (length(outcomes)) {
    observe outcomes[i] ~ Bernoulli(p);
  }
  return(p);
}
"""

with treeppl.Model(source=source, samples=10_000) as coin:
    res = coin(outcomes=[True, True, True, False, True, False, False, True, True, False, False, False, True, False, True, False, False, True, False, False])
    sns.histplot(x=res.samples, weights=res.nweights, bins=100, stat="density", kde=True)
    plt.show()
