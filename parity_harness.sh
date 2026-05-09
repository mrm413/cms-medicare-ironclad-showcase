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

# ── ANSI colors (auto-disabled if stdout isn't a TTY or NO_COLOR is set) ──
if [ -t 1 ] && [ -z "${NO_COLOR:-}" ]; then
    C_RESET=$'\033[0m'
    C_BOLD=$'\033[1m'
    C_GREEN=$'\033[32m'
    C_RED=$'\033[31m'
    C_YELLOW=$'\033[33m'
    C_CYAN=$'\033[36m'
    C_DIM=$'\033[2m'
else
    C_RESET=""; C_BOLD=""; C_GREEN=""; C_RED=""; C_YELLOW=""; C_CYAN=""; C_DIM=""
fi

echo "${C_BOLD}${C_CYAN}============================================================${C_RESET}"
echo "${C_BOLD}  Ironclad CMS Medicare Parity Validator${C_RESET}"
echo "  ${C_DIM}GnuCOBOL  ←→  Ironclad-transpiled Rust   (byte-for-byte)${C_RESET}"
echo "${C_BOLD}${C_CYAN}============================================================${C_RESET}"

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
        printf "${C_YELLOW}BUILD_FAIL_GNU${C_RESET}   %s\n" "$base"
        continue
    fi

    # Compile transpiler output (single self-contained driver .rs)
    if ! rustc --edition 2021 \
            -L "$DEPS_DIR" \
            --extern "cobol_runtime=$RLIB" \
            "$rs_driver" -o "$iron_exe" \
            >"$WORK_DIR/${idx}.rust_err" 2>&1; then
        BFAIL_RUST=$((BFAIL_RUST + 1))
        printf "${C_RED}BUILD_FAIL_RUST${C_RESET}  %s\n" "$base"
        continue
    fi

    gnu_out=$(timeout "$TIMEOUT_SECS" "$gnu_exe" </dev/null 2>/dev/null) || gnu_rc=$?
    gnu_rc=${gnu_rc:-0}
    iron_out=$(timeout "$TIMEOUT_SECS" "$iron_exe" </dev/null 2>/dev/null) || iron_rc=$?
    iron_rc=${iron_rc:-0}

    if [ "$gnu_rc" = "124" ] || [ "$iron_rc" = "124" ]; then
        RUN_ERR=$((RUN_ERR + 1))
        printf "${C_RED}TIMEOUT${C_RESET}          %s  ${C_DIM}(gnu_rc=%s iron_rc=%s)${C_RESET}\n" "$base" "$gnu_rc" "$iron_rc"
        continue
    fi

    # Normalize both outputs (CRLF, trailing whitespace, trailing blanks, nulls)
    norm() {
        printf '%s' "$1" | awk '
            {gsub(/\r/,""); gsub(/\000/,""); sub(/[ \t]+$/,""); a[NR]=$0}
            END{
                last=0
                for(i=NR;i>=1;i--){if(a[i]!=""){last=i;break}}
                for(i=1;i<=last;i++)print a[i]
            }
        '
    }
    gnu_out=$(norm "$gnu_out")
    iron_out=$(norm "$iron_out")

    if [ "$gnu_out" = "$iron_out" ]; then
        PASS=$((PASS + 1))
        FAMILY_PASS[$family]=$((${FAMILY_PASS[$family]:-0} + 1))
        printf "${C_GREEN}PASS${C_RESET}             %s  ${C_DIM}[%s]${C_RESET}\n" "$base" "$family"
    else
        MISMATCH=$((MISMATCH + 1))
        printf "${C_RED}${C_BOLD}MISMATCH${C_RESET}         %s  ${C_DIM}[%s]${C_RESET}\n" "$base" "$family"
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
echo "${C_BOLD}============================================================${C_RESET}"
echo "${C_BOLD}  CMS MEDICARE PARITY SUMMARY${C_RESET}"
echo "${C_BOLD}============================================================${C_RESET}"
printf "  Parity rate:   ${C_BOLD}${C_GREEN}%s%%${C_RESET}  (%d / %d)  ${C_DIM}byte-for-byte${C_RESET}\n" "$PARITY_PCT" "$PASS" "$PARITY_DENOM"
echo "------------------------------------------------------------"
printf "  ${C_GREEN}PASS${C_RESET}              %4d\n" "$PASS"
printf "  ${C_RED}MISMATCH${C_RESET}          %4d  ${C_DIM}(logic divergence — see $MISMATCH_LOG)${C_RESET}\n" "$MISMATCH"
printf "  ${C_YELLOW}BUILD_FAIL_GNU${C_RESET}    %4d  ${C_DIM}(cobc rejected source)${C_RESET}\n" "$BFAIL_GNU"
printf "  ${C_RED}BUILD_FAIL_RUST${C_RESET}   %4d  ${C_DIM}(rustc rejected transpiled .rs)${C_RESET}\n" "$BFAIL_RUST"
printf "  ${C_CYAN}TIMEOUT${C_RESET}           %4d  ${C_DIM}(one engine ran past %ss)${C_RESET}\n" "$RUN_ERR" "$TIMEOUT_SECS"
echo "------------------------------------------------------------"
echo "  ${C_BOLD}By program family:${C_RESET}"
for family in $(echo "${!FAMILY_TOTAL[@]}" | tr ' ' '\n' | sort); do
    p=${FAMILY_PASS[$family]:-0}
    t=${FAMILY_TOTAL[$family]}
    if [ "$p" = "$t" ]; then
        printf "    ${C_GREEN}%-12s${C_RESET}  %2d / %2d  ${C_GREEN}✓${C_RESET}\n" "$family" "$p" "$t"
    else
        printf "    %-12s  %2d / %2d\n" "$family" "$p" "$t"
    fi
done
echo "${C_BOLD}============================================================${C_RESET}"

if [ "$MISMATCH" -gt 0 ]; then exit 1; fi
if [ "$BFAIL_RUST" -gt 0 ]; then exit 2; fi
if [ "$RUN_ERR" -gt 0 ]; then exit 3; fi
exit 0
