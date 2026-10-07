import argparse
import json
import shutil
import subprocess
import sys
from pathlib import Path
from typing import NamedTuple

import yaml

import checks

SHARED_CONFIG = {
    "sample-size-flag": {
        "mcmc": "--iterations",
        "mcmc-naive": "--iterations",
        "mcmc-trace": "--iterations",
        "mcmc-graph": "--iterations",
        "pmcmc-pimh": "--iterations",
        "is": "--particles",
        "smc-bpf": "--particles",
        "smc-apf": "--particles",
    }
}

# Check function and distance name, keyed on the `discrete` config entry
CHECKS = {
    True: (checks.check_discrete_analytical, "TV"),
    False: (checks.check_continuous_analytical, "KS"),
}


class ReplicateResult(NamedTuple):
    seed: int
    passed: bool
    distance: float
    details: str


def load_config(model_dir: Path) -> dict:
    """Shared config with the model's config.yaml layered on top."""
    with open(model_dir / "config.yaml") as f:
        return SHARED_CONFIG | (yaml.safe_load(f) or {})


def compile_variant(bin: Path, model_dir: Path, variant: dict, config: dict) -> None:
    bin.parent.mkdir(parents=True, exist_ok=True)
    subprocess.run(
        [
            str(config["tpplc"]),
            str(model_dir / "model.tppl"),
            "--method",
            variant["method"],
            "--output",
            str(bin),
            *variant.get("additional-flags", "").split(),
        ],
        check=True,
        cwd=bin.parent,
        capture_output=True,
        text=True,
    )


def run_inference(
    bin: Path, model_dir: Path, variant: dict, seed: int, config: dict
) -> dict:
    """Run the compiled model and return its parsed JSON output."""
    result = subprocess.run(
        [
            str(bin),
            str(model_dir / "data.json"),
            config["sample-size-flag"][variant["method"]],
            str(variant["sample-size"]),
            "--seed",
            str(seed),
        ],
        check=True,
        capture_output=True,
        text=True,
    )
    return json.loads(result.stdout.splitlines()[-1])


def run_replicate(
    bin: Path, model_dir: Path, variant: dict, seed: int, config: dict
) -> ReplicateResult:
    check, _ = CHECKS[config["discrete"]]
    output = run_inference(bin, model_dir, variant, seed, config)
    distance, details = check(model_dir, output)
    return ReplicateResult(seed, distance < variant["threshold"], distance, details)


def run_variant(model_dir: Path, name: str, variant: dict, config: dict) -> bool:
    test_name = f"{model_dir.name}/{name}"
    bin = model_dir / "build" / name / "out"
    seeds = range(variant["seed"], variant["seed"] + variant["replicates"])
    try:
        compile_variant(bin, model_dir, variant, config)
        results = [
            run_replicate(bin, model_dir, variant, seed, config) for seed in seeds
        ]
    except subprocess.CalledProcessError as e:
        print(f"FAIL {test_name}: command exited with status {e.returncode}")
        print(f"  command: {' '.join(e.cmd)}")
        print(e.stderr)
        return False

    _, metric = CHECKS[config["discrete"]]
    threshold = variant["threshold"]
    npassed = sum(r.passed for r in results)
    all_passed = npassed == len(results)
    max_distance = max(r.distance for r in results)
    print(
        f"{'PASS' if all_passed else 'FAIL'} ({npassed}/{len(results)}) {test_name}: "
        f"max {metric} = {max_distance:.4f} (threshold {threshold})"
    )
    for r in results:
        if not r.passed:
            print(
                f"FAIL {test_name} seed {r.seed}: {metric} = {r.distance:.4f} (threshold {threshold})"
            )
            print(r.details)
    return all_passed


def main() -> None:
    parser = argparse.ArgumentParser("TreePPL test runner")
    parser.add_argument("model_directory", metavar="model-directory")
    parser.add_argument("--compiler-path", default="tpplc")
    args = parser.parse_args()
    model_dir = Path(args.model_directory).resolve()
    compiler = shutil.which(args.compiler_path)
    if compiler is None:
        sys.exit(f"Compiler not found: {args.compiler_path}")
    config = load_config(model_dir)
    config["tpplc"] = Path(compiler).absolute()

    results = [
        run_variant(model_dir, name, variant, config)
        for name, variant in config["inference"].items()
    ]
    sys.exit(0 if all(results) else 1)


if __name__ == "__main__":
    main()
