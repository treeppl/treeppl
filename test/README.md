# TreePPL inference tests
This subdirectory contains TreePPL inference tests for ensuring that our inference methods produce correct and accurate output.

## Running tests
The testing framework is written in Python and uses the [`uv` package manager](https://docs.astral.sh/uv/) for Python code execution.
Please follow the installation instructions on the [`uv` website](https://docs.astral.sh/uv/).
The tests also require the TreePPL compiler `tpplc` to be on your `PATH`.

To run all tests, simply run `make` in this subdirectory, or `make test` in the main TreePPL Makefile.
To run a single test, run `make <test name>`, e.g. `make beta-binom`.

## Testing approach
The tests compute a distance between a reference distribution and the empirical distribution based on the output from a TreePPL sampler.
The distance is then compared against a prespecified threshold.

If the output is discrete we use the total variation (TV) distance
$$
  TV(p^{(1)}, p^{(2)}) = \frac{1}{2} \sum_{s \in S} |p_s^{(1)} - p_s^{(2)}|
$$
for two probability mass functions $p^{(1)}, p^{(2)}$ with support on $S \subset \mathbb{Z}$ (the integers).
If the output is continuous we use the Kolmogorov-Smirnov (KS) distance
$$
  KS(F^{(1)}, F^{(2)}) = \sup_{x \in \mathbb{R}} |F^{(1)}(x) - F^{(2)}(x)|
$$
between two CDFs $F^{(1)}, F^{(2)}$.

A well-behaved sampler will converge in both metrics as the sample size $n \to \infty$.
However, when run for a finite amount of time there will be a discrepancy between the two distributions.
This means that the testing threshold must be set with care, so that biased inferences are caught and correct ones pass.
Note that the threshold applies to each replicate separately, so increasing the number of replicates also increases the chance that a correct sampler fails by chance.

## Adding a test
Each test is a subdirectory under `models/` together with an entry in the `TESTS` variable in the Makefile.
A test subdirectory has the following structure
```text
<test name>
├── config.yaml
├── data.json
├── model.tppl
└── <reference>
```

- The model file `model.tppl` contains a TreePPL model that exercises the behavior under test, e.g. a bug the test is added for.
It should contain an initial comment describing the reason for the test, and why this is an apt way of testing it.
The output should be summarized in a single `Real` or `Int`.

- The data file `data.json` contains a dataset that the model can be run with. It is generally preferred to encode the dataset in the model, and let the contents of the data file be an empty dictionary.

- The config file `config.yaml` contains metadata about the test.
It should contain the top-level keys:

  - `discrete: bool`: Is the output an `Int`?
  - `inference: dict`: The inference variants to run the test with. Each key is a free-form variant name (e.g. `mcmc-aligned-cps`), used for the build directory and in the test output. Each variant has the following keys:
    - `method: str`: Method name passed to the `--method` flag of `tpplc`
    - `additional-flags: str` (*optional*): Additional flags to pass to `tpplc`.
    - `sample-size: int`: Number of iterations/particles
    - `replicates: int`: Number of runs
    - `seed: int`: Initial seed. Seeds for replicate runs are given as `seed_i = seed + i - 1` for `i = 1, ..., replicates`.
    - `threshold: float`: The TV/KS threshold (see discussion above) for each replicate

- The structure of the reference distribution `<reference>` depends on the output type:
  - **discrete**, `analytical_pmf.json`: Supply a JSON file `analytical_pmf.json` with a dictionary containing two entries: `states` and `probs`, where `states` holds a list of integers and `probs` the corresponding probabilities. We expect both lists to be ordered in the same way. States not listed are assumed to have probability zero.
  - **continuous**, `analytical_cdf.py`: Supply a Python script `analytical_cdf.py` with a single function `def analytical_cdf(x: np.ndarray) -> np.ndarray` that outputs the CDF evaluated at each state in `x`. The function must be vectorized, e.g. by using `scipy.stats`.

  References based on samples rather than analytical expressions are not yet supported.

### Shared configuration
The file `test/config.yaml` holds settings shared by all tests.
In particular, `sample-size-flag` maps each `tpplc` method to the runtime flag that sets the sample size (`--iterations` or `--particles`).
If a test uses a method that is not listed there, add it.
