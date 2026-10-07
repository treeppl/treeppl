import scipy
import numpy as np


def normalized_weights(weights: list[float]) -> np.ndarray:
    """Normalize log-weights (as in TreePPL's output) to probabilities."""
    log_w = np.asarray(weights, dtype=float)
    return np.exp(log_w - scipy.special.logsumexp(log_w))


def empirical_pmf(samples: list, weights: list[float]) -> dict:
    """Weighted empirical PMF of `samples`, given log-weights `weights`."""
    probs = [p.item() for p in normalized_weights(weights)]

    # Merge weights of identical sample values
    pmf: dict = {}
    for s, p in zip(samples, probs):
        pmf[s] = pmf.get(s, 0.0) + p
    return {"states": list(pmf.keys()), "probs": list(pmf.values())}


def long_to_short_pmf(long_pmf):
    return {s: p for s, p in zip(long_pmf["states"], long_pmf["probs"])}


def tv_distance_discrete(pmf1: dict, pmf2: dict) -> float:
    pmf1 = long_to_short_pmf(pmf1)
    pmf2 = long_to_short_pmf(pmf2)
    tv = 0.0
    for k in pmf1.keys() | pmf2.keys():
        d1 = pmf1.get(k, 0.0)
        d2 = pmf2.get(k, 0.0)
        tv += abs(d1 - d2)
    return tv / 2


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
