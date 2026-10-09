"""Time Python chainladder on the same synthetic triangles as bench/benchmark.R.

Cross-language timings include different overheads (pandas/sparse array
set-up in chainladder) and are indicative only.
Run: python3 -I bench/python_reference.py
"""
import time, statistics, warnings
import numpy as np, pandas as pd
warnings.filterwarnings("ignore")
import chainladder as cl


def make_tri(n, seed=1):
    # Same construction as benchmark.R but numpy's RNG: values differ, sizes match.
    rng = np.random.default_rng(seed)
    pattern = np.diff(np.concatenate([[0], 1 - np.exp(-0.35 * np.arange(1, n + 1))]))
    rows = []
    for i in range(n):
        k = n - i
        vals = np.cumsum(1e6 * pattern[:k] * np.exp(rng.normal(0, 0.1, k)))
        for j in range(k):
            rows.append({"origin": 2000 + i, "valuation": 2000 + i + j, "value": vals[j]})
    df = pd.DataFrame(rows)
    return cl.Triangle(df, origin="origin", development="valuation", columns="value",
                       cumulative=True)


for n in [10, 25, 50, 100, 200]:
    tri = make_tri(n)
    for name, fn in [("chain_ladder", lambda: cl.Chainladder().fit(tri).ultimate_),
                     ("mack", lambda: cl.MackChainladder().fit(tri).total_mack_std_err_)]:
        ts = []
        for _ in range(5):
            t0 = time.perf_counter(); fn(); ts.append(time.perf_counter() - t0)
        print(f"{n},{name},{statistics.median(ts) * 1000:.3f}")
