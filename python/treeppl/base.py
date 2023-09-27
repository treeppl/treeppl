import json
from tempfile import TemporaryDirectory
from subprocess import Popen, PIPE, STDOUT
import numpy as np

from .exceptions import CompileError, InferenceError
from .serialization import from_json, to_json


class Model:
    def __init__(self, source=None, filename=None, method="smc-bpf", samples=1_000, **kwargs):
        self.temp_dir = TemporaryDirectory(prefix="treeppl_")
        if filename:
            source = open(filename).read()
        if not source:
            raise CompileError("No source code to compile.")
        with open(self.temp_dir.name + "/__main__.tppl", "w") as f:
            f.write(source)
        args = [
            "tpplc",
            "__main__.tppl",
            "-m", method,
        ]
        for k, v in kwargs.items():
            args.append(f"--{k.replace('_', '-')}")
            args.append(str(v))
        with Popen(
            args=args,
            cwd=self.temp_dir.name,
            stdout=PIPE,
            stderr=STDOUT
        ) as proc:
            proc.wait()
            if proc.returncode != 0:
                output = proc.stdout.read().decode('utf-8')
                output = output.replace("__main__.tppl", "source code")
                raise CompileError(f"Could not compile the TreePPL model:\n{output}")
        self.set_samples(samples)

    def set_samples(self, samples):
        self.samples = samples

    def __enter__(self):
        return self

    def __exit__(self, *args):
        self.temp_dir.cleanup()

    def __call__(self, **kwargs):
        with open(self.temp_dir.name + "/input.json", "w") as f:
            to_json(kwargs or {}, f)
        args = [
            f"{self.temp_dir.name}/out",
            f"{self.temp_dir.name}/input.json",
            str(self.samples)
        ]
        with Popen(args=args, stdout=PIPE) as proc:
            return InferenceResult(proc.stdout)

            
class InferenceResult:
    def __init__(self, stdout):
        try:
            result = from_json(stdout)
        except json.decoder.JSONDecodeError:
            raise InferenceError("Could not parse the output from TreePPL.")
        self.samples = result.get('samples', [])
        self.weights = np.array(result.get('weights', []))
        self.nweights = np.exp(self.weights)
        self.norm_const = result.get('normConst', np.nan)

    def subsample(self, size=1):
        idx = np.random.choice(len(self.nweights), size, p=self.nweights)
        return [self.samples[i] for i in idx]
