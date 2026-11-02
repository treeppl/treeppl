import importlib.util
from pathlib import Path

import common


def check_continuous_analytical(model_dir: Path, output: dict) -> tuple[float, str]:
    spec = importlib.util.spec_from_file_location(
        "analytical_cdf", model_dir / "analytical_cdf.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    ks = common.ks_distance_continuous(
        output["samples"], output["weights"], module.analytical_cdf
    )
    return ks, ""


def check_discrete_analytical(model_dir: Path, output: dict) -> tuple[float, str]:
    empirical = common.empirical_pmf(output["samples"], output["weights"])
    reference = common.load_pmf(model_dir / "analytical_pmf.json")
    tv = common.tv_distance_discrete(empirical, reference)
    details = f"  empirical: {empirical}\n  reference: {reference}"
    return tv, details
