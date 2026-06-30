#!/usr/bin/env python3
"""Aggregate per-benchmark hyperfine JSON into one markdown table.

Speedup = threaded_mean / tailcall_mean  (>1.0 means the tail-call interp is
faster). Reads every <name>.json in the build dir passed as argv[1]."""
import glob
import json
import os
import sys

build = sys.argv[1] if len(sys.argv) > 1 else "build"

rows = []
for path in sorted(glob.glob(os.path.join(build, "*.json"))):
    name = os.path.splitext(os.path.basename(path))[0]
    with open(path) as f:
        data = json.load(f)
    by = {r["command"]: r for r in data["results"]}
    if "threaded" not in by or "tailcall" not in by:
        continue
    th, tc = by["threaded"], by["tailcall"]
    speedup = th["mean"] / tc["mean"] if tc["mean"] else float("nan")
    rows.append((name, th, tc, speedup))

print("# Tail-call vs threaded-code bytecode interpreter\n")
print("Wall-clock time per benchmark, mean ± σ over hyperfine runs.")
print("Speedup = threaded / tailcall (higher = tail-call interp is faster).\n")
print("| benchmark | threaded (s) | tailcall (s) | speedup |")
print("|---|---:|---:|---:|")
geo = 1.0
n = 0
for name, th, tc, sp in rows:
    print(
        f"| `{name}` "
        f"| {th['mean']:.3f} ± {th['stddev']:.3f} "
        f"| {tc['mean']:.3f} ± {tc['stddev']:.3f} "
        f"| **{sp:.3f}×** |"
    )
    if sp == sp and sp > 0:  # not NaN
        geo *= sp
        n += 1
if n:
    print(f"\n**Geometric-mean speedup: {geo ** (1.0 / n):.3f}×** "
          f"over {n} benchmarks.")
