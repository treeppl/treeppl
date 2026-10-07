import common
import json
import importlib
from pathlib import Path


def check_continuous_analytical(
    model_dir: Path, output: dict
) -> tuple[str, float, str]:
    spec = importlib.util.spec_from_file_location(
        "analytical_cdf", model_dir / "analytical_cdf.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    ks = common.ks_distance_continuous(
        output["samples"], output["weights"], module.analytical_cdf
    )
    return ks, ""


def check_discrete_analytical(model_dir: Path, output: dict) -> tuple[str, float, str]:
    empirical = common.empirical_pmf(output["samples"], output["weights"])
    with open(model_dir / "analytical_pmf.json") as f:
        reference = json.load(f)
    tv = common.tv_distance_discrete(empirical, reference)
    details = (
        f"  empirical: {common.long_to_short_pmf(empirical)}\n"
        f"  reference: {common.long_to_short_pmf(reference)}"
    )
    return tv, details
