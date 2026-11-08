"""Generate the synthetic sample data in data/sample.

The numbers are invented. They only mimic the structure of the real inputs
(one row per region, one column per Solar Hijri year) so that the pipeline
can be run end to end without any confidential data.
"""
import csv
import os
import random

random.seed(42)
OUT = os.path.join(os.path.dirname(__file__), "..", "data", "sample")
os.makedirs(OUT, exist_ok=True)

HIST = list(range(1385, 1404))      # 19 historical years
FCST = list(range(1404, 1421))      # 17 forecast years
REGIONS = ["Region A", "Region B", "Region C", "Region D", "Region E", "Region F"]


def path(start, growth, years, noise):
    vals, v = [], start
    for _ in years:
        v *= 1 + growth + random.gauss(0, noise)
        vals.append(v)
    return vals


def price_path(years, base):
    # step increases, loosely like administered fuel price reforms
    steps = {1389: 3.0, 1393: 1.4, 1398: 2.0}
    vals, p = [], base
    for y in years:
        p *= steps.get(y, 1.0)
        vals.append(p)
    return vals


hist, fcst = {}, {}
for r in REGIONS:
    pop0 = random.uniform(0.6, 5.0)                  # million people
    inc0 = random.uniform(80, 160)
    car0 = pop0 * random.uniform(80, 140)            # thousand vehicles
    idx0 = random.uniform(40, 60)
    h = {
        "Population": path(pop0, 0.012, HIST, 0.002),
        "income": path(inc0, 0.015, HIST, 0.03),
        "Car": path(car0, 0.055, HIST, 0.01),
        "Index": path(idx0, 0.20, HIST, 0.03),
        "Price": price_path(HIST, 1000),
    }
    f = {
        "Population": path(h["Population"][-1], 0.010, FCST, 0.0),
        "income": path(h["income"][-1], 0.012, FCST, 0.0),
        "Car": path(h["Car"][-1], 0.035, FCST, 0.0),
        "Index": path(h["Index"][-1], 0.15, FCST, 0.0),
        "Price": [h["Price"][-1] * (1.08 ** (i + 1)) for i in range(len(FCST))],
    }
    # target: log-linear in the drivers plus noise (thousand litres)
    scale = random.uniform(2.0, 3.0)
    target = []
    for i in range(len(HIST)):
        v = (scale * h["Car"][i] ** 0.85 * h["income"][i] ** 0.25
             * h["Price"][i] ** -0.12 * 1000 * (1 + random.gauss(0, 0.02)))
        target.append(v)
    hist[r], fcst[r] = (h, target), f


def write(name, years, rows, total=False):
    with open(os.path.join(OUT, name), "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["Province"] + [str(y) for y in years])
        for r, vals in rows:
            w.writerow([r] + [f"{v:.4f}" for v in vals])
        if total:
            sums = [sum(vals[i] for _, vals in rows) for i in range(len(years))]
            w.writerow(["Total"] + [f"{v:.4f}" for v in sums])


write("Gasoline_main.csv", HIST, [(r, hist[r][1]) for r in REGIONS], total=True)
for var in ["Population", "income", "Car", "Index", "Price"]:
    write(f"{var}_hist.csv", HIST, [(r, hist[r][0][var]) for r in REGIONS])
    write(f"{var}_forecast.csv", FCST, [(r, fcst[r][var]) for r in REGIONS])
print("Synthetic data written to", os.path.abspath(OUT))
