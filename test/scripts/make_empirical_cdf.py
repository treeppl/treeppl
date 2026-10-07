import common
import json

data = json.loads(input())
pmf = common.empirical_pmf(data["samples"], data["weights"])
print(json.dumps(common.long_to_short_pmf(pmf)))
