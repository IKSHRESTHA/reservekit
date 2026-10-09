"""Cross-implementation reference values from Python chainladder 0.10.1.

Writes python_chainladder_reference.json and
python_chainladder_mack_nperiods.json next to this script. It shares no code
with the R package. Run from this directory with chainladder 0.10.1 installed:
    python3 chainladder_reference.py
then convert to test fixtures with `Rscript data-raw/make_fixtures.R`.
"""
import json
import warnings

import numpy as np
import chainladder as cl

warnings.filterwarnings("ignore")

# Exposure and a priori ELR used for BF and Cape Cod; the R tests use the same.
SETTINGS = {
    "raa": {"sample": "raa", "premium": np.linspace(30000, 48000, 10), "elr": 0.6},
    "genins": {"sample": "genins", "premium": np.linspace(8e6, 10e6, 10), "elr": 0.65},
}


def vec(x):
    """Flatten a single-index, single-column chainladder object to a list."""
    v = np.asarray(x.values if hasattr(x, "values") else x, dtype=float).ravel()
    return [float(a) for a in v]


def exposure_triangle(tri, premium):
    """Premium as a one-development-period triangle aligned with tri's origins."""
    ult = cl.Chainladder().fit(tri).ultimate_
    w = ult.copy()
    w.values = np.asarray(premium, dtype=float).reshape(w.values.shape)
    return w


def mack(tri, sigma, n_periods=-1):
    dev = cl.Development(n_periods=n_periods, sigma_interpolation=sigma).fit_transform(tri)
    m = cl.MackChainladder().fit(dev)
    # mack_std_err_ is by origin x age; the last column is the s.e. at ultimate.
    # chainladder can give NaN for a fully developed origin; its s.e. is 0.
    se = np.nan_to_num(np.asarray(m.mack_std_err_.values, dtype=float)[0, 0, :, -1])
    return [float(a) for a in se], float(np.asarray(m.total_mack_std_err_.values).ravel()[0])


def reference(name, s):
    tri = cl.load_sample(s["sample"])
    prem = exposure_triangle(tri, s["premium"])
    out = {
        "ldf_volume": vec(cl.Development(average="volume").fit(tri).ldf_),
        "ldf_simple": vec(cl.Development(average="simple").fit(tri).ldf_),
        "ldf_volume_n5": vec(cl.Development(average="volume", n_periods=5).fit(tri).ldf_),
    }
    out["mack_loglinear_se"], out["mack_loglinear_total_se"] = mack(tri, "log-linear")
    out["mack_mackinterp_se"], out["mack_mackinterp_total_se"] = mack(tri, "mack")
    out["premium"] = [float(p) for p in s["premium"]]
    bf = cl.BornhuetterFerguson(apriori=s["elr"]).fit(tri, sample_weight=prem)
    out["bf_ult"] = vec(bf.ultimate_)
    cc = cl.CapeCod(decay=1, trend=0).fit(tri, sample_weight=prem)
    out["cc_ult"] = vec(cc.ultimate_)
    out["cc_elr"] = vec(cc.apriori_)
    cc75 = cl.CapeCod(decay=0.75, trend=0).fit(tri, sample_weight=prem)
    out["cc_decay75_ult"] = vec(cc75.ultimate_)
    dc = tri.development_correlation(p_critical=0.5)
    out["devcorr_T"] = float(np.asarray(dc.t_expectation).ravel()[0])
    out["devcorr_var"] = float(np.asarray(dc.t_variance).ravel()[0])
    out["devcorr_range"] = None  # recorded as null in the committed reference
    vc = tri.valuation_correlation(p_critical=0.05, total=True)
    out["valcorr"] = {
        "z": vec(vc.z),
        "z_critical": [bool(b) for b in np.asarray(vc.z_critical.values).ravel()],
        "z_expectation": vec(vc.z_expectation),
        "z_variance": vec(vc.z_variance),
        "range": [float(np.asarray(r).ravel()[0]) for r in vc.range],
    }
    tail = cl.TailCurve(curve="exponential").fit(cl.Development().fit_transform(tri))
    out["tail_exponential"] = float(np.asarray(tail.tail_.values).ravel()[0])
    out["tail_cdf"] = vec(tail.cdf_)
    return out, tri


def nperiods(tri, name):
    out = {}
    for n in (3, 5):
        for sigma in ("mack", "log-linear"):
            se, total = mack(tri, sigma, n_periods=n)
            out[f"{name}_n{n}_{sigma}"] = {"se": se, "total_se": total}
    return out


ref, nper = {}, {}
for name, s in SETTINGS.items():
    ref[name], tri = reference(name, s)
    nper.update(nperiods(tri, name))
nper = {k: nper[k] for k in [f"{t}_n{n}_{s}" for t in SETTINGS for n in (3, 5)
                             for s in ("mack", "log-linear")]}

with open("python_chainladder_reference.json", "w") as f:
    json.dump(ref, f, indent=1)
with open("python_chainladder_mack_nperiods.json", "w") as f:
    json.dump(nper, f, indent=1)
