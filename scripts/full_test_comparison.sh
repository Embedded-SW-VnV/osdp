#!/bin/sh
# Full solver x basis x padding comparison, across both osdp's own test
# suite and a real downstream project (saimcop). Both write rows to the
# same accumulating osdp.trace log (report/trace.jsonl): nothing is ever
# truncated, so re-running this script builds up history rather than
# replacing it. Regenerates report/full_test_comparison.html from the
# full accumulated log each time.
#
# Usage: scripts/full_test_comparison.sh   (or `make fulltest_comparison`)
#
# Configurable via environment variables:
#   MOSEK_BIN_DIR   - Mosek's tools/platform/<arch>/bin directory, added to
#                     PATH when set (unset: Mosek must already be reachable)
#   SAIMCOP_DIR     - path to saimcop's code/approx directory (unset: the
#                     saimcop rows are skipped)
#   OSDP_LOCAL_INSTALL - scratch prefix for a local (non-opam) osdp install

set -e

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
OSDP_DIR=$(cd "$SCRIPT_DIR/.." && pwd)
cd "$OSDP_DIR"

: "${MOSEK_BIN_DIR:=}"
: "${SAIMCOP_DIR:=}"
: "${OSDP_LOCAL_INSTALL:=$OSDP_DIR/_local_install}"

if [ -d "$MOSEK_BIN_DIR" ]; then
  PATH="$MOSEK_BIN_DIR:$PATH"
  export PATH
fi

REPORT_DIR="$OSDP_DIR/report"
mkdir -p "$REPORT_DIR"
LOG="$REPORT_DIR/trace.jsonl"

echo "== [1/5] refreshing configure (solver autodetection) =="
autoconf
./configure > "$REPORT_DIR/configure.log" 2>&1 || {
  echo "configure failed, see $REPORT_DIR/configure.log" >&2
  exit 1
}
grep -E "^  (Compiled with|Default solver)" "$REPORT_DIR/configure.log"

echo "== [2/5] building osdp and running its full matrix =="
dune build 2>&1 | tee "$REPORT_DIR/dune_build.log"
rm -rf "$OSDP_LOCAL_INSTALL"
dune install osdp --prefix="$OSDP_LOCAL_INSTALL" > "$REPORT_DIR/dune_install.log" 2>&1
dune build test/matrix_runner.exe 2>&1 | tee -a "$REPORT_DIR/dune_build.log"
./_build/default/test/matrix_runner.exe "$LOG"

echo "== [3/5] rebuilding saimcop's residual_probe against this osdp =="
if [ -d "$SAIMCOP_DIR" ]; then
  ( cd "$SAIMCOP_DIR" \
    && ocamlbuild -clean \
    && OCAMLPATH="$OSDP_LOCAL_INSTALL/lib" ocamlbuild -use-ocamlfind residual_probe.native \
    > "$REPORT_DIR/saimcop_build.log" 2>&1 ) || {
    echo "saimcop residual_probe build failed, see $REPORT_DIR/saimcop_build.log" >&2
    exit 1
  }

  echo "== [4/5] running saimcop's matrix (same solver x basis x pad grid) =="
  for solver in csdp mosek sdpa sdpa_gmp sdpa_dd; do
    for basis in classical hermite hermite_normalized; do
      for pad in 2 0; do
        PROBE_SOLVER="$solver" PROBE_BASIS="$basis" PROBE_PAD="$pad" PROBE_LOG="$LOG" \
          "$SAIMCOP_DIR/residual_probe.native" 2>>"$REPORT_DIR/saimcop_run.log"
      done
    done
  done
else
  echo "SAIMCOP_DIR ($SAIMCOP_DIR) not found, skipping saimcop rows" >&2
fi

echo "== [5/5] generating HTML report from the accumulated log =="
python3 "$OSDP_LOCAL_INSTALL/share/osdp/gen_report.py" "$LOG" "$REPORT_DIR/full_test_comparison.html"

echo "Done."
echo "  Log:  $LOG (accumulates across runs)"
echo "  HTML: $REPORT_DIR/full_test_comparison.html"
