import json
from pathlib import Path

import scipy
import numpy as np

# A probability mass function, mapping each state to its probability
PMF = dict[int, float]


def normalized_weights(weights: list[float]) -> np.ndarray:
    """Normalize log-weights (as in TreePPL's output) to probabilities."""
    log_w = np.asarray(weights, dtype=float)
    return np.exp(log_w - scipy.special.logsumexp(log_w))


def empirical_pmf(samples: list[int], weights: list[float]) -> PMF:
    """Weighted empirical PMF of `samples`, given log-weights `weights`."""
    pmf: PMF = {}
    # Merge weights of identical sample values
    for s, p in zip(samples, normalized_weights(weights)):
        pmf[s] = pmf.get(s, 0.0) + p.item()
    return pmf


def load_pmf(path: Path) -> PMF:
    """Load a PMF stored as JSON with paired `states` and `probs` lists."""
    with open(path) as f:
        data = json.load(f)
    return dict(zip(data["states"], data["probs"]))


def tv_distance_discrete(pmf1: PMF, pmf2: PMF) -> float:
    return sum(abs(pmf1.get(k, 0.0) - pmf2.get(k, 0.0)) for k in pmf1.keys() | pmf2.keys()) / 2


def ks_distance_continuous(samples: list[float], weights: list[float], cdf) -> float:
    """KS distance sup_x |F_n(x) - F(x)| between the weighted empirical CDF of
    `samples` and the reference CDF `cdf` (vectorized over numpy arrays)."""
    order = np.argsort(samples)
    x = np.asarray(samples, dtype=float)[order]
    w = normalized_weights(weights)[order]
    above = np.cumsum(w)  # F_n just after each sample
    below = above - w  # F_n just before each sample
    ref = cdf(x)
    return float(max(np.max(np.abs(above - ref)), np.max(np.abs(below - ref))))
