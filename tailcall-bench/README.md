# Tail-call interpreter benchmark

Measures the `tailcall-interp` bytecode interpreter (`preserve_none` + `musttail`,
many functions) against the stock **threaded-code** interpreter (labels-as-values,
one big function) — the real baseline on platforms that support computed gotos.

## Why this is a fair comparison

OCaml bytecode is **interpreter-agnostic**: a `.byte` file only needs *an*
`ocamlrun` with a matching magic number. So we build **two** `ocamlrun` binaries
from this one source tree, differing *only* in the dispatch loop in
`runtime/interp.c`, compile each benchmark to bytecode **once**, and run the
identical bytecode under both. The compiler, stdlib, GC and C primitives are
byte-for-byte identical between the two runs — the only variable is dispatch.

The two flavours are selected by `WANT_TAIL_CALL_INTERP` in
`runtime/caml/s.h` (set by `./configure --enable-tail-call-interp`). `run.sh`
toggles that macro, rebuilds the runtime, and restores your tree afterwards.

## Requirements

- An OCaml source tree (this harness lives in `<tree>/tailcall-bench`).
  It does **not** need to be configured or built first — `run.sh` does that.
- A C compiler with `musttail`/`preserve_none` (Clang 19+ / GCC 15+), so the
  tail-call runtime can be built. `configure` checks this.
- `hyperfine` (`brew install hyperfine` / `apt install hyperfine`)
- `python3` — optional, only for the combined summary table; without it the
  per-benchmark hyperfine tables are still printed.

Portable across macOS and Linux (no GNU-only flags; `make -j` is auto-sized).

## Run it

From a bare clone — no prior `./configure`/`make` needed:

```sh
cd tailcall-bench
./run.sh            # configure (if needed), build both runtimes,
                    # compile benches, check agreement, benchmark, report
```

`run.sh` configures the tree with `--enable-tail-call-interp` only if it isn't
already, builds the world once, then builds the two runtimes by toggling
`WANT_TAIL_CALL_INTERP` in `s.h` (restoring your tree afterward).

Sub-steps (each re-runnable on its own):

```sh
./run.sh runtimes   # build build/ocamlrun.tailcall and build/ocamlrun.threaded
./run.sh compile    # compile bench/*.ml -> build/*.byte (once)
./run.sh bench      # hyperfine both runtimes on every benchmark (wall-clock)
./run.sh perf       # Linux only: perf stat hardware counters -> results_perf.md
./run.sh report     # aggregate build/*.json -> results.md
```

Output:

- `build/ocamlrun.tailcall`, `build/ocamlrun.threaded` — the two runtimes
- `build/<name>.byte` — the shared bytecode
- `build/<name>.{json,md}` — per-benchmark hyperfine results
- `results.md` — combined table with per-benchmark and geometric-mean speedup

`run.sh` first checks that **both runtimes produce identical output** for every
benchmark before timing — a cheap guard against a miscompiled interpreter.

## Hardware counters (`perf`, Linux)

Wall-clock is noisy and machine-specific. `./run.sh perf` runs `perf stat -r N`
on each benchmark under both runtimes and compares the counters — these
*attribute* a speedup instead of just measuring it:

- **instructions retired** — deterministic; the tail-call interp's whole point is
  to dispatch with fewer instructions, so this is the headline. `>1.00×` (the
  table's ratio is threaded/tailcall) means tail-call does less work.
- **cycles** — the real cost; should track wall-clock.
- **IPC** (instructions/cycle) — front-end efficiency.
- **branch-miss rate** — interpreter dispatch lives or dies by indirect-branch
  prediction; regressions here often explain a slowdown that has *fewer*
  instructions.
- **L1-icache-load-misses** — added automatically when supported; tests the
  code-layout / I-cache hypotheses (e.g. whether outlining or inlining handlers
  helped or hurt).

Output goes to `results_perf.md` plus per-run `build/<name>.<flavour>.perfcsv`.

Requirements: a real Linux host with counter access — `perf` installed and
`kernel.perf_event_paranoid` ≤ 2 (`sudo sysctl kernel.perf_event_paranoid=1`),
or run as root. Most VMs/containers need `--privileged` (or PMU passthrough) to
read PMU counters; `run.sh perf` prints a warning if it can't. Tune the repeat
count with `PERF_REPS` and the event list with `PERF_EVENTS`.

## Knobs

| env | default | meaning |
|---|---|---|
| `OCAMLSRC` | `..` | path to the OCaml source tree |
| `CONFIGURE_FLAGS` | — | extra `./configure` flags (the TC flag is added for you) |
| `JOBS` | detected | parallel `make` jobs |
| `ORP` | `s=4M` | `OCAMLRUNPARAM`, applied identically to both runtimes |
| `WARMUP` | `3` | hyperfine warmup runs |
| `MINRUNS` | `12` | hyperfine minimum runs |
| `PERF_REPS` | `5` | `perf stat -r` repeat count |
| `PERF_EVENTS` | core set | comma-separated `perf` events (I-cache appended if available) |

Benchmark sizes live in the `BENCHES` array at the top of `run.sh`; tune them so
each run is ~0.3–2 s on your machine.

## The benchmarks

| file | character |
|---|---|
| `fib`, `tak`, `ack` | pure integer recursion — **dispatch-bound** (best case) |
| `loop` | tight integer loop — purest raw-dispatch measure |
| `nqueens` | recursion + branchy bit-logic — dispatch-bound |
| `sieve` | bounds-checked `Bytes` access in tight loops |
| `lists` | closure application + minor-heap allocation (mixed) |
| `binarytrees` | allocation/GC-bound — **realistic floor** of the speedup |
| `spectralnorm` | float-array tight loops (mixed) |

Each program takes its size as `argv`, wraps the result in
`Sys.opaque_identity`, and prints it, so nothing is optimised away and both
runtimes can be checked for identical output.

## Caveats

- No `perf` on macOS; this relies on hyperfine wall-clock. Close other apps;
  expect a couple-% wander on a laptop.
- The tail-call win concentrates in dispatch-bound code; allocation/GC-bound
  benchmarks (e.g. `binarytrees`) will show a smaller margin — that's expected
  and worth reporting alongside the microbenchmarks.
- Always compare bytecode built by the **same** `ocamlc`. `run.sh` does this.
