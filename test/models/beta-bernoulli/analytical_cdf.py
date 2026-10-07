import scipy


def analytical_cdf(x):
    return scipy.stats.beta.cdf(x, 3, 2)
