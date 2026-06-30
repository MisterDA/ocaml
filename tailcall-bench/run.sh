#!/usr/bin/env bash
#
# Benchmark the tail-call bytecode interpreter against the threaded-code
# interpreter. Portable across macOS/Linux; runnable from a bare OCaml clone.
#
# Idea: bytecode is interpreter-agnostic, so we build *two* ocamlrun binaries
# from the same tree -- one with the tail-call interp (preserve_none/musttail),
# one with the threaded-code interp (labels-as-values) -- compile each benchmark
# to bytecode ONCE, and run the identical .byte under both runtimes. The only
# variable is the dispatch loop in runtime/interp.c.
#
# Zero-setup usage from a fresh clone (this harness lives in <tree>/tailcall-bench):
#
#     ./run.sh
#
# It will, as needed: ./configure --enable-tail-call-interp, build the world,
# build both runtimes, compile the benchmarks, check both runtimes agree, then
# benchmark with hyperfine and emit results.md.
#
# Sub-commands (each re-runnable on its own):
#   ./run.sh setup      # configure the tree (only if not already configured)
#   ./run.sh runtimes   # build build/ocamlrun.tailcall and build/ocamlrun.threaded
#   ./run.sh compile    # compile bench/*.ml -> build/*.byte (once)
#   ./run.sh bench      # hyperfine both runtimes on every benchmark
#   ./run.sh report     # aggregate build/*.json -> results.md
#
# Env:
#   OCAMLSRC          OCaml source tree (default: the harness's parent dir)
#   CONFIGURE_FLAGS   extra flags for ./configure (the TC flag is added for you)
#   JOBS              parallel make jobs (default: detected core count)
#   ORP               OCAMLRUNPARAM applied identically to both runtimes (s=4M)
#   WARMUP / MINRUNS  hyperfine warmup / minimum runs (3 / 12)
#
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
OCAMLSRC="${OCAMLSRC:-$(cd "$HERE/.." && pwd)}"
BUILD="$HERE/build"
BENCHDIR="$HERE/bench"
SH="$OCAMLSRC/runtime/caml/s.h"
ORP="${ORP:-s=4M}"
WARMUP="${WARMUP:-3}"
MINRUNS="${MINRUNS:-12}"
CONFIGURE_FLAGS="${CONFIGURE_FLAGS:-}"
PERF_REPS="${PERF_REPS:-5}"
# Hardware counters for the perf mode. The first five are ~universally available;
# L1-icache-load-misses is appended automatically when the CPU/perf supports it
# (it tests the code-layout / I-cache hypotheses for interpreter changes).
PERF_EVENTS="${PERF_EVENTS:-task-clock,cycles,instructions,branches,branch-misses}"
# CPU to pin benchmarks to during `perf` mode.  Pinning both variants to the
# same physical core makes cycles and instruction counts directly comparable.
# On hybrid Intel CPUs (Alder/Raptor Lake) the E-cores have no hyper-threading
# and are the most stable for micro-benchmarking; we auto-detect them.
# Override with e.g. PERF_CPU=4 to force a specific logical CPU.
PERF_CPU="${PERF_CPU:-}"

# Portable core count for `make -j`.
if [ -z "${JOBS:-}" ]; then
  JOBS="$( (command -v nproc >/dev/null 2>&1 && nproc) \
        || sysctl -n hw.ncpu 2>/dev/null || echo 4)"
fi

RT_TC="$BUILD/ocamlrun.tailcall"
RT_TH="$BUILD/ocamlrun.threaded"

# Benchmarks and their (optional) arguments. Tune sizes here for ~0.3-2s/run.
BENCHES=(
  "fib 35"
  "tak 8"
  "ack 3 8"
  "loop 100000000"
  "nqueens 11"
  "sieve 2000000 20"
  "lists 2000 800"
  "binarytrees 16"
  "spectralnorm 500"
)

log()  { printf '\033[1;34m==>\033[0m %s\n' "$*"; }
warn() { printf '\033[1;33m!!\033[0m %s\n'  "$*" >&2; }
die()  { printf '\033[1;31mxx\033[0m %s\n'  "$*" >&2; exit 1; }

# ---------------------------------------------------------------------------
# Pick a single logical CPU for perf pinning.
# Preference: an E-core (max_freq <= 4000 MHz) — these have no hyper-threading
# on Intel hybrid designs, giving the cleanest measurements.  Falls back to
# the last available CPU if no E-core is found.
detect_perf_cpu() {
  local best="" max_avail=""
  for dir in /sys/devices/system/cpu/cpu[0-9]*/cpufreq; do
    [ -f "$dir/cpuinfo_max_freq" ] || continue
    local cpu freq
    cpu="${dir%/cpufreq}"; cpu="${cpu##*/cpu}"
    freq="$(cat "$dir/cpuinfo_max_freq")"
    # Prefer E-cores (freq <= 4 000 000 kHz)
    if [ -z "$best" ] && [ "$freq" -le 4000000 ]; then
      best="$cpu"
    fi
    max_avail="$cpu"   # keep updating; last one wins as fallback
  done
  echo "${best:-${max_avail:-0}}"
}

mkdir -p "$BUILD"

domake() { ( cd "$OCAMLSRC" && make -j"$JOBS" "$@" ); }

# Ensure the bytecode toolchain (ocamlc + stdlib) exists. We only need it to
# *compile* the benchmarks; it is interpreter-agnostic, so it is built once and
# reused for both runtimes. On a bare clone this does the full bootstrap.
ensure_toolchain() {
  if [ -x "$OCAMLSRC/ocamlc" ] && [ -f "$OCAMLSRC/stdlib/stdlib.cma" ]; then
    log "toolchain present (ocamlc + stdlib); skipping full build"
    return
  fi
  log "Bootstrapping toolchain (full make -j$JOBS) ..."
  domake > "$BUILD/build.toolchain.log" 2>&1 \
    || { tail -40 "$BUILD/build.toolchain.log" >&2; die "toolchain build failed (see $BUILD/build.toolchain.log)"; }
}

# ---------------------------------------------------------------------------
# Ensure the tree is configured WITH the tail-call interp, so s.h carries the
# `#define WANT_TAIL_CALL_INTERP 1` line that build_runtimes toggles.
setup() {
  if [ ! -f "$OCAMLSRC/configure" ]; then
    die "no $OCAMLSRC/configure -- not an OCaml source tree (or run autoconf first)"
  fi
  if [ -f "$OCAMLSRC/Makefile.config" ] \
     && grep -q '^#define WANT_TAIL_CALL_INTERP 1' "$SH" 2>/dev/null; then
    log "tree already configured with the tail-call interp"
    return
  fi
  log "Configuring ($OCAMLSRC): --enable-tail-call-interp $CONFIGURE_FLAGS"
  ( cd "$OCAMLSRC" && ./configure --enable-tail-call-interp $CONFIGURE_FLAGS ) \
    > "$BUILD/configure.log" 2>&1 \
    || { tail -40 "$BUILD/configure.log" >&2; die "configure failed (see $BUILD/configure.log)"; }
  grep -q '^#define WANT_TAIL_CALL_INTERP 1' "$SH" \
    || die "configure did not enable WANT_TAIL_CALL_INTERP -- does your compiler \
support the musttail/preserve_none attributes (Clang 19+/GCC 15+)?"
}

# ---------------------------------------------------------------------------
build_one_runtime() {  # $1 = tailcall|threaded ; $2 = dest path
  local flavour="$1" dest="$2"
  case "$flavour" in
    tailcall) cp "$SH.bench-bak" "$SH" ;;                       # pristine: TC on
    threaded) sed -e 's|^#define WANT_TAIL_CALL_INTERP 1|/* WANT_TAIL_CALL_INTERP disabled by tailcall-bench */|' \
                  "$SH.bench-bak" > "$SH" ;;                    # TC off -> threaded
    *) die "unknown flavour $flavour" ;;
  esac
  # Toggling WANT_TAIL_CALL_INTERP also flips THREADED_CODE (config.h), which
  # several runtime files compile against (interp.c, fix_code.c, startup_byt.c,
  # meta.c, callback.c). `make runtime/ocamlrun` does NOT load header-dependency
  # info, so a stale object compiled in the other config would silently mismatch
  # (e.g. a threaded-code fix_code.o linked into a tail-call runtime -> crash).
  # Force a clean, consistent rebuild of the whole runtime in the current config.
  rm -f "$OCAMLSRC"/runtime/*.b.o "$OCAMLSRC"/runtime/prims.o "$OCAMLSRC/runtime/ocamlrun"
  # Targeted: rebuild ONLY the bytecode runtime (-Werror is on). This avoids the
  # full-world `make`, which would also relink the compiler -- slower, and prone
  # to wedging in `make ocaml`.
  log "Building $flavour runtime (make -j$JOBS runtime/ocamlrun) ..."
  domake runtime/ocamlrun > "$BUILD/build.$flavour.log" 2>&1 \
    || { tail -40 "$BUILD/build.$flavour.log" >&2; die "make failed for $flavour (see $BUILD/build.$flavour.log)"; }
  cp "$OCAMLSRC/runtime/ocamlrun" "$dest"
  log "  -> $dest"
}

build_runtimes() {
  setup
  ensure_toolchain
  [ -f "$SH" ] || die "no $SH"
  grep -q '^#define WANT_TAIL_CALL_INTERP 1' "$SH" \
    || die "s.h lacks WANT_TAIL_CALL_INTERP=1 after setup -- cannot build the TC flavour"
  cp "$SH" "$SH.bench-bak"
  trap 'cp "$SH.bench-bak" "$SH"; rm -f "$SH.bench-bak"' EXIT  # always restore the tree

  build_one_runtime threaded "$RT_TH"
  build_one_runtime tailcall "$RT_TC"   # also restores pristine s.h, leaves tree on TC

  trap - EXIT
  cp "$SH.bench-bak" "$SH"; rm -f "$SH.bench-bak"

  # Sanity: tail-call binary must carry tc_handler_* symbols, threaded must not.
  # (Use grep -c, never `grep -q` in a pipe under `set -o pipefail`: -q closes
  # the pipe early, nm gets SIGPIPE, and the pipeline reports false failure.)
  if command -v nm >/dev/null 2>&1; then
    local n_tc n_th
    n_tc="$(nm "$RT_TC" 2>/dev/null | grep -c tc_handler_ || true)"
    n_th="$(nm "$RT_TH" 2>/dev/null | grep -c tc_handler_ || true)"
    [ "$n_tc" -gt 0 ] && log "sanity: $n_tc tc_handler_* symbols in tailcall runtime" \
                      || warn "sanity: no tc_handler_* in $RT_TC (stripped, or not a TC build?)"
    [ "$n_th" -eq 0 ] || warn "sanity: tc_handler_* unexpectedly present in $RT_TH"
  fi
}

# ---------------------------------------------------------------------------
compile_benches() {
  [ -x "$OCAMLSRC/ocamlc" ] || die "no $OCAMLSRC/ocamlc -- run: ./run.sh runtimes"
  [ -f "$OCAMLSRC/stdlib/stdlib.cma" ] || die "stdlib not built -- run: ./run.sh runtimes"
  for spec in "${BENCHES[@]}"; do
    local name="${spec%% *}"
    log "Compiling $name.ml -> $name.byte"
    "$OCAMLSRC/runtime/ocamlrun" "$OCAMLSRC/ocamlc" \
      -nostdlib -I "$OCAMLSRC/stdlib" "$OCAMLSRC/stdlib/stdlib.cma" \
      "$BENCHDIR/$name.ml" -o "$BUILD/$name.byte"
  done
}

# ---------------------------------------------------------------------------
run_bench() {
  command -v hyperfine >/dev/null 2>&1 || die "hyperfine not found (brew/apt install hyperfine)"
  [ -x "$RT_TC" ] && [ -x "$RT_TH" ] || die "runtimes missing; run: ./run.sh runtimes"
  export OCAMLRUNPARAM="$ORP"
  log "OCAMLRUNPARAM=$OCAMLRUNPARAM  (identical for both runtimes)"

  for spec in "${BENCHES[@]}"; do
    set -- $spec; local name="$1"; shift
    local o_tc o_th
    o_tc="$("$RT_TC" "$BUILD/$name.byte" "$@")"
    o_th="$("$RT_TH" "$BUILD/$name.byte" "$@")"
    [ "$o_tc" = "$o_th" ] || die "OUTPUT MISMATCH on $name: tc='$o_tc' threaded='$o_th'"
  done
  log "correctness: both runtimes agree on all benchmark outputs"

  for spec in "${BENCHES[@]}"; do
    set -- $spec; local name="$1"; shift
    log "Benchmarking $name ${*:+(args: $*)}"
    hyperfine --warmup "$WARMUP" --min-runs "$MINRUNS" -N \
      --command-name threaded "$RT_TH $BUILD/$name.byte $*" \
      --command-name tailcall "$RT_TC $BUILD/$name.byte $*" \
      --export-json "$BUILD/$name.json" \
      --export-markdown "$BUILD/$name.md"
  done
}

# ---------------------------------------------------------------------------
# Linux-only: collect hardware counters with `perf stat`. Unlike wall-clock,
# instructions-retired and branch-misses are deterministic and machine-
# independent, so they attribute *why* one interpreter is faster (fewer
# dispatched instructions? better branch prediction? fewer I-cache misses?).
perf_run() {
  case "$(uname -s)" in
    Linux) : ;;
    *) die "perf mode is Linux-only (no perf on $(uname -s)); use ./run.sh bench for wall-clock" ;;
  esac
  command -v perf >/dev/null 2>&1 || die "perf not found (install linux-tools-common / linux-perf)"
  command -v taskset >/dev/null 2>&1 || die "taskset not found (install util-linux)"
  command -v python3 >/dev/null 2>&1 || die "perf aggregation needs python3"
  [ -x "$RT_TC" ] && [ -x "$RT_TH" ] || die "runtimes missing; run: ./run.sh runtimes"

  # Ensure bytecode files exist; compile on demand so `./run.sh perf` is self-contained.
  if ! ls "$BUILD"/*.byte >/dev/null 2>&1; then
    log "byte files missing; compiling benchmarks first"
    compile_benches
  fi

  export OCAMLRUNPARAM="$ORP"

  # Resolve the CPU to pin to.  Both variants must run on the same physical
  # core so that instruction and cycle counts are directly comparable.
  local cpu="${PERF_CPU}"
  if [ -z "$cpu" ]; then
    cpu="$(detect_perf_cpu)"
    log "auto-selected CPU $cpu for perf pinning (override with PERF_CPU=N)"
  fi
  log "pinning both runtimes to CPU $cpu (taskset -c $cpu)"

  # Append I-cache misses if this CPU/perf exposes them (best-effort).
  local events="$PERF_EVENTS"
  if perf list 2>/dev/null | grep -q 'L1-icache-load-misses'; then
    events="$events,L1-icache-load-misses"
  fi
  log "perf events: $events   (perf stat -r $PERF_REPS, OCAMLRUNPARAM=$OCAMLRUNPARAM)"

  # quick perf sanity: can we actually count? (containers/VMs often can't)
  if ! perf stat -x, -e instructions -- true >/dev/null 2>&1; then
    warn "perf cannot access counters here (need kernel.perf_event_paranoid<=2, or run as root, \
or --privileged in a container). Trying anyway."
  fi

  # Correctness check: both runtimes must produce identical output.
  for spec in "${BENCHES[@]}"; do
    set -- $spec; local name="$1"; shift
    local o_tc o_th
    o_tc="$(taskset -c "$cpu" "$RT_TC" "$BUILD/$name.byte" "$@")"
    o_th="$(taskset -c "$cpu" "$RT_TH" "$BUILD/$name.byte" "$@")"
    [ "$o_tc" = "$o_th" ] || die "OUTPUT MISMATCH on $name: tc='$o_tc' threaded='$o_th'"
  done
  log "correctness: both runtimes agree on all benchmark outputs"

  for spec in "${BENCHES[@]}"; do
    set -- $spec; local name="$1"; shift
    for fl in threaded tailcall; do
      local rt; if [ "$fl" = tailcall ]; then rt="$RT_TC"; else rt="$RT_TH"; fi
      log "perf $name [$fl] (CPU $cpu)"
      # taskset pins the process to the chosen CPU so both variants run on the
      # same core type.  On hybrid Intel CPUs this prevents the scheduler from
      # migrating the process between P-cores and E-cores, which would produce
      # incomparable cycle and instruction counts.
      # -o writes the CSV stats; the program's own stdout is discarded.
      perf stat -x, -r "$PERF_REPS" -e "$events" -o "$BUILD/$name.$fl.perfcsv" \
        -- taskset -c "$cpu" "$rt" "$BUILD/$name.byte" "$@" \
        >/dev/null 2>"$BUILD/$name.$fl.perferr" \
        || warn "perf failed for $name/$fl (see $BUILD/$name.$fl.perferr)"
    done
  done

  python3 "$HERE/aggregate_perf.py" "$BUILD" > "$HERE/results_perf.md"
  log "Wrote $HERE/results_perf.md"; echo; cat "$HERE/results_perf.md"
}

# ---------------------------------------------------------------------------
report() {
  if command -v python3 >/dev/null 2>&1; then
    python3 "$HERE/aggregate.py" "$BUILD" > "$HERE/results.md"
    log "Wrote $HERE/results.md"; echo; cat "$HERE/results.md"
  else
    warn "python3 not found; skipping combined table. Per-benchmark results:"
    cat "$BUILD"/*.md 2>/dev/null || true
  fi
}

# ---------------------------------------------------------------------------
case "${1:-all}" in
  setup)    setup ;;
  runtimes) build_runtimes ;;
  compile)  compile_benches ;;
  bench)    run_bench ;;
  perf)     perf_run ;;
  report)   report ;;
  all)      build_runtimes; compile_benches; run_bench; report ;;
  *)        die "unknown command: $1 (use: all|setup|runtimes|compile|bench|perf|report)" ;;
esac
