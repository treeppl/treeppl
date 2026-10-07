import importlib.util
import json
import subprocess
import sys
from pathlib import Path
import argparse
import shutil

import yaml

import common
import checks

SHARED_CONFIG = {
    "sample-size-flag": {
        "mcmc": "--iterations",
        "mcmc-naive": "--iterations",
        "mcmc-trace": "--iterations",
        "mcmc-graph": "--iterations",
        "is": "--particles",
        "smc-bpf": "--particles",
        "smc-apf": "--particles",
    }
}


def load_config(model_dir: Path) -> dict:
    """Shared config with the model's config.yaml layered on top."""
    with open(model_dir / "config.yaml") as f:
        config = yaml.safe_load(f) or {}
    config |= SHARED_CONFIG
    return config


def compile_variant(model_dir: Path, bin: Path, variant: dict, config: dict) -> None:
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
    )


def run_inference(
    model_dir: Path,
    bin: Path,
    method_name: str,
    sample_size: int,
    seed: int,
    config: dict,
) -> dict:
    """Run the compiled model and return its parsed JSON output."""
    result = subprocess.run(
        [
            str(bin),
            str(model_dir / "data.json"),
            config["sample-size-flag"][method_name],
            str(sample_size),
            "--seed",
            str(seed),
        ],
        check=True,
        capture_output=True,
        text=True,
    )
    return json.loads(result.stdout.splitlines()[-1])


CHECKS = {
    # discrete
    True: (checks.check_discrete_analytical, "TV"),
    False: (checks.check_continuous_analytical, "KS"),
}


def run_variant(model_dir: Path, name: str, variant: dict, config: dict):
    # Compile the model
    bin = model_dir / "build" / name / "out"
    compile_variant(model_dir, bin, variant, config)
    seed = variant["seed"]

    threshold = variant["threshold"]
    check, metric = CHECKS[config["discrete"]]

    instances = [
        run_variant_instance(model_dir, bin, s, threshold, check, variant, config)
        for s in range(seed, seed + variant["replicates"])
    ]
    npassed = sum(passed for passed, _, _, _ in instances)
    max_dist = max(distance for _, _, distance, _ in instances)
    all_passed = npassed == len(instances)
    status = "PASS" if all_passed else "FAIL"
    print(
        f"{status} ({npassed}/{len(instances)}) {model_dir.name}/{name}: max {metric} = {max_dist:.4f} (threshold {threshold})"
    )
    if not all_passed:
        for passed, details, distance, seed in instances:
            if not passed:
                print(
                    f"{status} {model_dir.name}/{name} seed {seed}: {metric} = {distance:.4f} (threshold {threshold})"
                )
                print(details)
    return all_passed


def run_variant_instance(
    model_dir: Path,
    bin: Path,
    seed: int,
    threshold: float,
    check,
    variant: dict,
    config: dict,
) -> bool:
    output = run_inference(
        model_dir, bin, variant["method"], variant["sample-size"], seed, config
    )
    distance, details = check(model_dir, output)
    passed = distance < threshold

    return (passed, details, distance, seed)


def main() -> None:
    parser = argparse.ArgumentParser("TreePPL test runner")
    parser.add_argument("model_directory", metavar="model-directory")
    parser.add_argument("--compiler-path", default="tpplc")
    args = parser.parse_args()
    model_dir = Path(args.model_directory).resolve()
    compiler = shutil.which(args.compiler_path)
    if compiler is None:
        sys.exit(f"Compiler not found: {args.compiler_path}")
    compiler = Path(compiler).resolve()
    config = load_config(model_dir)
    config["tpplc"] = compiler

    results = []
    for name, variant in config["inference"].items():
        replicates = variant.get("replicates", 4)
        status = run_variant(model_dir, name, variant, config)
        results.append(status)
    sys.exit(0 if all(results) else 1)


if __name__ == "__main__":
    main()
