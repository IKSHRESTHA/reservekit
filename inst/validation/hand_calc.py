"""Independent reference calculation for the small validation triangle.

Uses exact rational arithmetic (fractions.Fraction) and a direct transcription
of Mack (1993) with an explicit double loop over origin pairs for the total.
It shares no code with the R package. Run: python3 hand_calc.py
"""
from fractions import Fraction as Fr
import math, json

C = [[100, 150, 180, 190],
     [110, 165, 200, None],
     [120, 186, None, None],
     [130, None, None, None]]
I = 4  # origins, also number of ages
C = [[None if v is None else Fr(v) for v in row] for row in C]

# volume-weighted factors f_k, k = 0..I-2 (0-based)
f = []
for k in range(I - 1):
    rows = [i for i in range(I) if C[i][k + 1] is not None]
    f.append(sum(C[i][k + 1] for i in rows) / sum(C[i][k] for i in rows))

# sigma^2
s2 = []
for k in range(I - 1):
    rows = [i for i in range(I) if C[i][k + 1] is not None]
    if len(rows) >= 2:
        s2.append(sum(C[i][k] * (C[i][k + 1] / C[i][k] - f[k]) ** 2 for i in rows) / (len(rows) - 1))
    else:
        s2.append(None)
for k in range(I - 1):
    if s2[k] is None:
        a, b = s2[k - 1], s2[k - 2]
        s2[k] = min(a * a / b, b, a)

# projections
latest_idx = [max(j for j in range(I) if C[i][j] is not None) for i in range(I)]
Chat = [row[:] for row in C]
for i in range(I):
    for j in range(latest_idx[i], I - 1):
        Chat[i][j + 1] = Chat[i][j] * f[j]
ult = [Chat[i][I - 1] for i in range(I)]
latest = [C[i][latest_idx[i]] for i in range(I)]
reserve = [ult[i] - latest[i] for i in range(I)]
S = [sum(C[i][k] for i in range(I) if C[i][k + 1] is not None) for k in range(I - 1)]

mse = []
for i in range(I):
    m = Fr(0)
    for k in range(latest_idx[i], I - 1):
        m += s2[k] / f[k] ** 2 * (1 / Chat[i][k] + 1 / S[k])
    mse.append(ult[i] ** 2 * m)

tot = sum(mse)
for i in range(I):
    for l in range(i + 1, I):  # l younger than i: shares steps of origin i
        for k in range(latest_idx[i], I - 1):
            tot += 2 * ult[i] * ult[l] * s2[k] / f[k] ** 2 / S[k]

out = {
    "factors": [float(x) for x in f],
    "sigma2": [float(x) for x in s2],
    "ultimate": [float(x) for x in ult],
    "reserve": [float(x) for x in reserve],
    "mack_se": [math.sqrt(float(x)) for x in mse],
    "total_reserve": float(sum(reserve)),
    "total_mack_se": math.sqrt(float(tot)),
    "factors_exact": [str(x) for x in f],
}
print(json.dumps(out, indent=1))
