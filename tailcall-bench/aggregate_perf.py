#!/usr/bin/env python3
"""Aggregate `perf stat -x,` CSV files into one markdown table.

For each benchmark we read <name>.threaded.perfcsv and <name>.tailcall.perfcsv
(produced by `run.sh perf`) and compare the hardware counters. Instructions
retired and branch-misses are deterministic and machine-independent, so they
explain *why* one interpreter wins, not just by how much.

Ratio columns are threaded/tailcall, so >1.00 always means the tail-call
interpreter is better (fewer instructions / cycles / misses). IPC and the
branch-miss rate are shown as threaded -> tailcall."""
import glob
import os
import sys

build = sys.argv[1] if len(sys.argv) > 1 else "build"


def parse(path):
    out = {}
    try:
        f = open(path)
    except OSError:
        return out
    with f:
        for line in f:
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            cols = line.split(",")
            if len(cols) < 3:
                continue
            try:
                val = float(cols[0])
            except ValueError:
                continue
            # On hybrid CPUs perf reports separate cpu_atom/* and cpu_core/*
            # counters for the same logical event.  Skip entries where the
            # process spent 0% of time on that core type: when perf stat -r N
            # averages runs that occasionally migrated, it produces a non-zero
            # value for the PMU of the core visited briefly.  Those spurious
            # readings would inflate the total; only sum entries where the
            # process actually ran on that core type.
            try:
                time_running_pct = float(cols[5]) if len(cols) > 5 else 100.0
            except ValueError:
                time_running_pct = 100.0
            if time_running_pct == 0.0:
                continue
            event = cols[2].strip().rstrip("/")
            # Normalize PMU-specific names (e.g. cpu_atom/cycles/ -> cycles).
            if "/" in event:
                event = event.split("/")[-1]
            out[event] = out.get(event, 0) + val
    return out


def names():
    ns = []
    for p in sorted(glob.glob(os.path.join(build, "*.tailcall.perfcsv"))):
        n = os.path.basename(p)[: -len(".tailcall.perfcsv")]
        if os.path.exists(os.path.join(build, n + ".threaded.perfcsv")):
            ns.append(n)
    return ns


def ratio(a, b):
    return a / b if (a is not None and b not in (None, 0)) else None


def human(x):
    if x is None:
        return "—"
    for div, suf in ((1e9, "G"), (1e6, "M"), (1e3, "k")):
        if abs(x) >= div:
            return f"{x/div:.2f}{suf}"
    return f"{x:.0f}"


def cell(r):
    return f"{r:.3f}×" if r is not None else "—"


rows = []
have_icache = False
for n in names():
    th, tc = parse(os.path.join(build, n + ".threaded.perfcsv")), \
             parse(os.path.join(build, n + ".tailcall.perfcsv"))
    g = lambda d, k: d.get(k)
    ins_t, ins_c = g(th, "instructions"), g(tc, "instructions")
    cyc_t, cyc_c = g(th, "cycles"), g(tc, "cycles")
    brm_t, brm_c = g(th, "branch-misses"), g(tc, "branch-misses")
    br_t, br_c = g(th, "branches"), g(tc, "branches")
    ic_t, ic_c = g(th, "L1-icache-load-misses"), g(tc, "L1-icache-load-misses")
    if ic_t is not None or ic_c is not None:
        have_icache = True
    ipc = lambda i, c: (i / c) if (i and c) else None
    rate = lambda m, b: (100.0 * m / b) if (m is not None and b) else None
    rows.append(dict(
        n=n,
        ins=ratio(ins_t, ins_c), cyc=ratio(cyc_t, cyc_c),
        ipc_t=ipc(ins_t, cyc_t), ipc_c=ipc(ins_c, cyc_c),
        bmr_t=rate(brm_t, br_t), bmr_c=rate(brm_c, br_c),
        ic=ratio(ic_t, ic_c),
    ))

print("# Tail-call vs threaded-code interpreter — hardware counters (perf)\n")
print(f"Means over `perf stat -r` repeats. Ratio columns are **threaded / tailcall** "
      f"(>1.00 ⇒ tail-call is better). IPC and branch-miss rate shown threaded → tailcall.\n")

hdr = "| benchmark | insns ↓ | cycles ↓ | IPC (th→tc) | br-miss% (th→tc) |"
sep = "|---|---:|---:|---:|---:|"
if have_icache:
    hdr += " icache-miss ↓ |"
    sep += "---:|"
print(hdr)
print(sep)

import math
gins = gcyc = 0.0
nins = ncyc = 0
for r in rows:
    line = (f"| `{r['n']}` | {cell(r['ins'])} | {cell(r['cyc'])} "
            f"| {r['ipc_t']:.2f} → {r['ipc_c']:.2f} "
            f"| {r['bmr_t']:.2f} → {r['bmr_c']:.2f} |"
            if r['ipc_t'] and r['ipc_c'] and r['bmr_t'] is not None and r['bmr_c'] is not None
            else f"| `{r['n']}` | {cell(r['ins'])} | {cell(r['cyc'])} | — | — |")
    if have_icache:
        line += f" {cell(r['ic'])} |"
    print(line)
    if r['ins']: gins += math.log(r['ins']); nins += 1
    if r['cyc']: gcyc += math.log(r['cyc']); ncyc += 1

if nins and ncyc:
    print(f"\n**Geomean — instructions: {math.exp(gins/nins):.3f}×, "
          f"cycles: {math.exp(gcyc/ncyc):.3f}×** (threaded/tailcall; >1 ⇒ tail-call better).")
