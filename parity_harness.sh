#!/bin/bash
# ============================================================================
#  Ironclad CMS Medicare Parity Validator
# ============================================================================
#
#  For every Medicare PPS Pricer test driver in this repo:
#    1. GnuCOBOL  (cobc -x driver.cob program.cob …)  →  reference exe
#    2. Ironclad  (rustc test_X.rs)                   →  transpiled exe
#    3. Run both with the driver-supplied bill-record input
#    4. Byte-for-byte stdout diff. PASS / MISMATCH / BUILD_FAIL
#
#  Streams each result LIVE — federal evaluator watches green ticks scroll.
#  Final summary by program family (LTCH, ESRD, SNF, Hospice, HHA, IPF, IRF).
#
#  Usage:
#    bash parity_harness.sh                          # all programs
#    bash parity_harness.sh --quick 20               # first 20 only
#    bash parity_harness.sh --filter LTCAL           # LTCH-family only
#    docker build -t medicare-parity -f Dockerfile.parity .
#    docker run --rm medicare-parity
# ============================================================================

set -uo pipefail

DRIVERS_DIR="cobol_source/test_drivers"
COBOL_DIR="cobol_source"
RUST_DIR="rust_drivers"
RUNTIME_DIR="cobol-runtime"
WORK_DIR="_parity_work"
RESULTS_DIR="parity_results"
TIMEOUT_SECS=10

QUICK_LIMIT=0
FILTER=""
SKIP_BUILD=0
while [[ $# -gt 0 ]]; do
    case "$1" in
        --quick)    QUICK_LIMIT="${2:-20}"; shift 2 ;;
        --filter)   FILTER="$2"; shift 2 ;;
        --no-build) SKIP_BUILD=1; shift ;;
        --timeout)  TIMEOUT_SECS="$2"; shift 2 ;;
        -h|--help)  sed -n '1,25p' "$0"; exit 0 ;;
        *)          echo "unknown arg: $1"; exit 2 ;;
    esac
done

echo "============================================================"
echo "  Ironclad CMS Medicare Parity Validator"
echo "  GnuCOBOL ←→ Ironclad-transpiled Rust  (byte-for-byte)"
echo "============================================================"

if ! command -v cobc >/dev/null 2>&1; then
    echo "ERROR: cobc not found. Install GnuCOBOL 3.x first."
    exit 2
fi
if ! command -v rustc >/dev/null 2>&1; then
    echo "ERROR: rustc not found. Install Rust toolchain (stable 1.70+)."
    exit 2
fi

echo "  cobc:  $(cobc --version | head -1)"
echo "  rustc: $(rustc --version)"
echo

if [ "$SKIP_BUILD" -eq 0 ]; then
    echo "[setup] Building cobol-runtime (release)…"
    (cd "$RUNTIME_DIR" && cargo build --release 2>&1 | tail -2) || {
        echo "ERROR: cobol-runtime failed to build"
        exit 2
    }
fi

RLIB=$(ls "$RUNTIME_DIR"/target/release/deps/libcobol_runtime-*.rlib 2>/dev/null | head -1)
if [ -z "$RLIB" ]; then
    echo "ERROR: libcobol_runtime-*.rlib not found"
    exit 2
fi
DEPS_DIR="$RUNTIME_DIR/target/release/deps"
echo "  rlib:  $(basename "$RLIB")"
echo

mkdir -p "$WORK_DIR" "$RESULTS_DIR"
trap 'rm -rf "$WORK_DIR"' EXIT

# Each test = test_X.cob + matching X.cob called program + test_X.rs
TESTS=()
for driver in "$DRIVERS_DIR"/test_*.cob; do
    [ -f "$driver" ] || continue
    base=$(basename "$driver" .cob)              # test_ltcal202
    called=${base#test_}                          # ltcal202
    upper=$(echo "$called" | tr '[:lower:]' '[:upper:]')   # LTCAL202
    called_cob="$COBOL_DIR/${upper}.cob"
    [ -f "$called_cob" ] || continue
    rs_driver="$RUST_DIR/${base}.rs"
    [ -f "$rs_driver" ] || continue
    if [ -n "$FILTER" ] && [[ "$base" != *"$FILTER"* ]] && [[ "$called" != *"$FILTER"* ]]; then continue; fi
    TESTS+=("$base|$driver|$called_cob|$rs_driver")
done

if [ "$QUICK_LIMIT" -gt 0 ]; then
    TESTS=("${TESTS[@]:0:$QUICK_LIMIT}")
fi

TOTAL="${#TESTS[@]}"
if [ "$TOTAL" -eq 0 ]; then
    echo "No test drivers found in $DRIVERS_DIR"
    exit 2
fi

echo "[run] $TOTAL Medicare program parity tests selected"
echo "------------------------------------------------------------"

PASS=0
MISMATCH=0
BFAIL_GNU=0
BFAIL_RUST=0
RUN_ERR=0
declare -A FAMILY_PASS
declare -A FAMILY_TOTAL

MISMATCH_LOG="$RESULTS_DIR/mismatches.txt"
> "$MISMATCH_LOG"

idx=0
for entry in "${TESTS[@]}"; do
    idx=$((idx + 1))
    IFS='|' read -r base driver_cob called_cob rs_driver <<< "$entry"

    # Family detection from program name (LTCAL202 → LTCAL → LTCH family)
    called_upper=$(basename "$called_cob" .cob)
    family=$(echo "$called_upper" | sed -E 's/[0-9]+$//')   # LTCAL, ESCAL, etc.
    FAMILY_TOTAL[$family]=$((${FAMILY_TOTAL[$family]:-0} + 1))

    gnu_exe="$WORK_DIR/${idx}_gnu"
    iron_exe="$WORK_DIR/${idx}_iron"

    printf "[%3d/%d] " "$idx" "$TOTAL"

    # Compile reference: driver + called program in one cobc invocation
    if ! cobc -x -fixed -frelax-syntax -findirect-redefines -I "$COBOL_DIR" \
            -o "$gnu_exe" "$driver_cob" "$called_cob" \
            >"$WORK_DIR/${idx}.gnu_err" 2>&1; then
        BFAIL_GNU=$((BFAIL_GNU + 1))
        echo "BUILD_FAIL_GNU   $base"
        continue
    fi

    # Compile transpiler output (single self-contained driver .rs)
    if ! rustc --edition 2021 \
            -L "$DEPS_DIR" \
            --extern "cobol_runtime=$RLIB" \
            "$rs_driver" -o "$iron_exe" \
            >"$WORK_DIR/${idx}.rust_err" 2>&1; then
        BFAIL_RUST=$((BFAIL_RUST + 1))
        echo "BUILD_FAIL_RUST  $base"
        continue
    fi

    gnu_out=$(timeout "$TIMEOUT_SECS" "$gnu_exe" </dev/null 2>/dev/null) || gnu_rc=$?
    gnu_rc=${gnu_rc:-0}
    iron_out=$(timeout "$TIMEOUT_SECS" "$iron_exe" </dev/null 2>/dev/null) || iron_rc=$?
    iron_rc=${iron_rc:-0}

    if [ "$gnu_rc" = "124" ] || [ "$iron_rc" = "124" ]; then
        RUN_ERR=$((RUN_ERR + 1))
        echo "TIMEOUT          $base  (gnu_rc=$gnu_rc iron_rc=$iron_rc)"
        continue
    fi

    if [ "$gnu_out" = "$iron_out" ]; then
        PASS=$((PASS + 1))
        FAMILY_PASS[$family]=$((${FAMILY_PASS[$family]:-0} + 1))
        echo "PASS             $base"
    else
        MISMATCH=$((MISMATCH + 1))
        echo "MISMATCH         $base"
        {
            echo "=== $base ==="
            echo "--- GnuCOBOL ---"
            printf '%s\n' "$gnu_out"
            echo "--- Ironclad ---"
            printf '%s\n' "$iron_out"
            echo
        } >> "$MISMATCH_LOG"
    fi

    rm -f "$gnu_exe" "$iron_exe" "$WORK_DIR/${idx}.gnu_err" "$WORK_DIR/${idx}.rust_err"
done

PARITY_DENOM=$((PASS + MISMATCH))
PARITY_PCT="0.0"
if [ "$PARITY_DENOM" -gt 0 ]; then
    PARITY_PCT=$(awk "BEGIN{printf \"%.1f\", $PASS*100/$PARITY_DENOM}")
fi

echo
echo "============================================================"
echo "  CMS MEDICARE PARITY SUMMARY"
echo "============================================================"
printf "  Parity rate:   %s%%  (%d / %d)  byte-for-byte\n" "$PARITY_PCT" "$PASS" "$PARITY_DENOM"
echo "------------------------------------------------------------"
printf "  PASS              %4d\n" "$PASS"
printf "  MISMATCH          %4d  (logic divergence — see $MISMATCH_LOG)\n" "$MISMATCH"
printf "  BUILD_FAIL_GNU    %4d  (cobc rejected source)\n" "$BFAIL_GNU"
printf "  BUILD_FAIL_RUST   %4d  (rustc rejected transpiled .rs)\n" "$BFAIL_RUST"
printf "  TIMEOUT           %4d  (one engine ran past %ss)\n" "$RUN_ERR" "$TIMEOUT_SECS"
echo "------------------------------------------------------------"
echo "  By program family:"
for family in $(echo "${!FAMILY_TOTAL[@]}" | tr ' ' '\n' | sort); do
    p=${FAMILY_PASS[$family]:-0}
    t=${FAMILY_TOTAL[$family]}
    printf "    %-12s  %2d / %2d\n" "$family" "$p" "$t"
done
echo "============================================================"

if [ "$MISMATCH" -gt 0 ]; then exit 1; fi
if [ "$BFAIL_RUST" -gt 0 ]; then exit 2; fi
if [ "$RUN_ERR" -gt 0 ]; then exit 3; fi
exit 0
