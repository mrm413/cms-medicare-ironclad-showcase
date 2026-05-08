# Ironclad: CMS Medicare Payment System — Byte-for-Byte Parity

**17 years of CMS Medicare pricer programs (FY2005 – FY2021) — byte-for-byte parity across SNF, ESRD, Hospice, Home Health, IPF, IRF, and LTCH families | Zero external dependencies | No AI**

This repository contains the **output** of the Ironclad transpilation system applied to real, production-grade CMS (Centers for Medicare & Medicaid Services) payment system COBOL. Every `.rs` file here was generated automatically from production Medicare pricer source code, and every test in this repo runs both the original COBOL and the generated Rust on the same inputs and diffs their outputs **byte for byte**.

Ironclad is a proprietary transpilation engine built by [Torsova LLC](https://torsova.com). The source code for Ironclad is not included in this repository.

---

## Why This Matters

These are not test programs. They are the actual COBOL that runs on CMS mainframes to determine how much Medicare pays hospitals, dialysis centers, hospice providers, skilled nursing facilities, inpatient psychiatric facilities, inpatient rehabilitation facilities, home health agencies, and long-term care hospitals.

The COBOL has characteristics that break most transpilers:

- **Deeply chained REDEFINES** — fields redefining fields that redefine other fields, three levels deep
- **COMP-3 packed decimal** — mainframe BCD arithmetic (`PIC S9(11)V99 COMP-3`) with the exact sign-nibble layout
- **Mixed level numbers** — `10 B-COV-CHARGES` followed by `05 B-REVIEW-CODE` (mainframe IBM COBOL accepts; strict modern compilers reject)
- **Long field names** — `PPS-SITE-NEUTRAL-IPPS-PMT` (25 chars) — strict compilers cap at 24
- **Deep copybook hierarchies** — multi-level COPY REPLACING with cross-program shared data structures
- **Multi-program CALL linkage** — driver programs that CALL pricers, pricers that CALL utility programs, with LINKAGE SECTION threading
- **Mixed numeric types** — COMP, COMP-3, DISPLAY numeric, DISPLAY-NUMERIC SIGN LEADING SEPARATE, with implicit conversion between all of them

Ironclad handles all of this deterministically and produces Rust whose output matches the COBOL output to the byte.

---

## Coverage

The CMS pricer source in this repository spans **EFFECTIVE dates from 1998 through 2021** — 17 active fiscal years (FY2005 – FY2021) of payment-rate logic. The pricer families covered:

| Pricer System | What It Calculates | Programs Included |
|---|---|---|
| **ESRD** (End-Stage Renal Disease) | Dialysis facility per-treatment payment rates | 21 (ESCAL FY2007 – FY2021 + ESDRV200 + ESDRV212) |
| **LTCH** (Long-Term Care Hospital) | Long-term acute-care DRG pricing, site-neutral payments | 27 LTCAL programs |
| **IRF** (Inpatient Rehabilitation Facility) | IRF PPS payment rates | 20 IRCAL programs (FY2007 – FY2021) + IRDRV200 |
| **IPF** (Inpatient Psychiatric Facility) | IPF PPS payment rates | 23 IPCAL programs + IPDRV |
| **SNF** (Skilled Nursing Facility) | PDPM nursing facility payment rates | SNFPR190, SNFPR210, SNFDR211 |
| **Hospice** | Hospice per-diem payment rates | HOSPR210 + HOSDR210 |
| **Home Health** | HH PPS payment rates | HHCAL200 + HHCAL213 |

---

## Parity Result

Each test case runs the original COBOL through GnuCOBOL, runs the Ironclad-generated Rust binary on the same input, and compares stdout byte for byte.

| Family | Tested fiscal year(s) | Result |
|---|---|---|
| **SNF — FY2021** (3 test cases) | FY2021 | ✅ Byte-for-byte MATCH |
| **ESRD — ESDRV200, ESDRV212** | FY2007 baseline | ✅ Byte-for-byte MATCH |
| **ESRD — ESCAL FY2007 → FY2021** (20 test cases) | FY2007 – FY2021 | ✅ Byte-for-byte MATCH on all 20 |
| **Hospice — HOSPR210** | FY2021 | ✅ Byte-for-byte MATCH |
| **Home Health — HHCAL200, HHCAL213** | FY2020, FY2021 | ✅ Byte-for-byte MATCH |
| **IPF — IPCAL220** | FY2022 | ✅ Byte-for-byte MATCH |
| **IRF — IRCAL201 batch** | FY2021 | ✅ Byte-for-byte MATCH |
| **LTCH — LTCAL family sweep** (27 programs) | FY2005 – FY2020 | ✅ **27 / 27 byte-for-byte MATCH** |
| **LTCH — LTCAL202** | FY2020 | ✅ Ironclad-only validated (¹) |
| **SNF — SNFPR190** | FY2019 | ✅ Ironclad-only validated (¹) |

**60+ byte-for-byte parity test cases across 7 pricer families and 17 fiscal years.**

(¹) **"Ironclad-only validated":** GnuCOBOL refuses to compile LTCAL202 / SNFPR190 because of strict mainframe constructs the GnuCOBOL toolchain doesn't accept (mixed level numbers, chained REDEFINES, long field names — see `parity_results/LTCH_PARITY_NOTE.md` for the exact errors). The Ironclad-transpiled Rust **does compile and run cleanly**, and produces output that matches the documented LTCH / SNF return codes (e.g. `PPS-CALC-VERS-CD = V20.2` corresponding to the actual `CAL-VERSION VALUE` constant in LTCAL202.cob). For these two programs, GnuCOBOL is not available as a side-by-side reference because GnuCOBOL itself rejects the source — this is a limitation of the reference compiler, not the transpiler.

---

## What This Proves

- **Ironclad accepts mainframe COBOL constructs that strict modern compilers reject** — mixed level numbers, chained REDEFINES, long field names, EJECT directives, FILLER VALUE table init.
- **Byte-for-byte parity holds across program families.** A fix that closes ESDRV212 also held up across SNF (FY2021 ×3), all 20 ESCAL fiscal years, Hospice (FY2021), Home Health (FY2020/FY2021), IPF (FY2022), and IRF (IRCAL201 batch) — no new bugs surfaced when the same transpiler was pointed at a different family.
- **18 GnuCOBOL edge cases** were catalogued during this work — patterns where the stock GnuCOBOL test suite doesn't cover real CMS production code (chained REDEFINES, mixed level numbers, EJECT, FILLER VALUE table init, etc.). These are documented but **do not block parity**, because Ironclad accepts the constructs natively.

---

## Reproducing the Result

The Docker harness streams a live color-coded log — green PASS ticks for every program where Rust matches COBOL byte for byte, red MISMATCH for divergences, yellow BUILD_FAIL_GNU when GnuCOBOL itself rejects mainframe-strict source. At the end you get a per-family summary (LTCAL, ESCAL, IRCAL, …).

```bash
# Build the parity validator image (one-time)
docker build -t ironclad-cms-parity -f Dockerfile.parity .

# Full sweep with live color stream — pass `-it` for the green-tick experience
docker run --rm -it ironclad-cms-parity

# Filter to one pricer family
docker run --rm -it ironclad-cms-parity bash parity_harness.sh --filter ESCAL
docker run --rm -it ironclad-cms-parity bash parity_harness.sh --filter LTCAL
docker run --rm -it ironclad-cms-parity bash parity_harness.sh --filter SNF
docker run --rm -it ironclad-cms-parity bash parity_harness.sh --filter HOSP

# Plain mode (no TTY, no color, still streams — for CI pipes)
docker run --rm ironclad-cms-parity
```

Each run produces:
- The COBOL output (from GnuCOBOL `cobc -x` + execute)
- The Rust output (from `rustc` + execute on the Ironclad-generated `.rs`)
- A `cmp -s` byte-for-byte diff
- A streaming pass/fail tag for every test case

---

## Type Mapping

| COBOL | Rust |
|-------|------|
| `PIC X(N)` | `FixedString<N>` |
| `PIC 9(N)` | `u32` / `u64` |
| `PIC S9(N)` | `i32` / `i64` |
| `PIC S9(N)V9(M)` | exact fixed-point Decimal |
| `PIC S9(N) COMP` | `i16` / `i32` / `i64` |
| `PIC S9(N) COMP-3` | packed BCD with exact sign-nibble layout |
| `88-level` | enum variant |
| `OCCURS N TIMES` | `[T; N]` |
| `OCCURS DEPENDING ON` | `Vec<T>` |
| `REDEFINES` | struct overlay |
| `FD file-name` | sequential / indexed / relative file handle |

The runtime library is a single pure Rust crate with **zero external dependencies** — no FFI, no C bindings, no `unsafe` blocks in either the runtime or any of the generated programs.

---

## Looking at the Output

### COBOL Input (SNFDR211 — SNF Driver, excerpt)

```cobol
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.    SNFDR211.
000300*AUTHOR.        CMS.
000800*REMARKS. (CENTERS FOR MEDICARE AND MEDICAID SERVICES)
000900***         - NATIONAL SNF PRICER EFFECTIVE OCT 1, 2020
001000***         - SNF PRICER REFERS TO A PROGRAM WHICH WILL
001100***           CALCULATE THE MEDICARE RATE UPON WHICH THE
001200***           PDPM SNF PPS PAYMENT IS MADE.
```

### Rust Output (excerpt — generated)

```rust
#[derive(Debug, Clone, Default, PartialEq)]
pub struct CbsaWageIndexRecord {
    /// CBSA-WIR-CBSA
    pub cbsa_wir_cbsa: FixedString<5>,
    /// CBSA-WIR-EFFDATE
    pub cbsa_wir_effdate: FixedString<8>,
    /// CBSA-WIR-AREA-WAGEIND
    pub cbsa_wir_area_wageind: Decimal,   // 2,4 unsigned
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct SnfInputData {
    /// SNF-MSA
    pub snf_msa: FixedString<4>,
    /// SNF-CBSA
    pub snf_cbsa: SnfCbsa,
    /// SNF-SPEC-WI-IND
    pub snf_spec_wi_ind: FixedString<1>,
}
```

Every COBOL data structure becomes a Rust struct, with a doc comment preserving the original field name. The validator runs both the COBOL and the Rust on the same SNF claim input and confirms the resulting payment rate is identical to the byte.

---

## What Makes This Different

1. **Real production code.** Not toy benchmarks. These are the actual CMS Medicare pricers that determine payment rates for millions of Medicare claims.
2. **Mainframe-strict COBOL accepted.** Mixed level numbers, chained REDEFINES, 25-character field names, EJECT directives — Ironclad accepts what GnuCOBOL refuses.
3. **17 fiscal years validated.** A single transpilation pipeline that holds across 17 years of payment-rate logic without per-year tuning.
4. **Byte-for-byte parity.** Not "looks right." The Rust output has to match the COBOL output to the byte before the test passes.
5. **Zero dependencies.** Pure Rust runtime, no FFI, no C bindings, no `unsafe`.

---

## Related Showcases

| Repo | What it shows |
|------|---|
| [Ironclad-COBOL-to-Rust](https://github.com/mrm413/Ironclad-COBOL-to-Rust) | GnuCOBOL 3.2 in-scope test corpus — 835 / 835 byte-for-byte parity (100%) |
| [ironclad-carddemo-showcase](https://github.com/mrm413/ironclad-carddemo-showcase) | AWS CardDemo CICS / COBOL — 44/44 transpiled, production CICS runtime + React 3270 UI |
| [cms-medicare-lazarus-showcase](https://github.com/mrm413/cms-medicare-lazarus-showcase) | C++17 sibling: same CMS Medicare COBOL transpiled to hardened C++17 |

---

## Built By

**Torsova LLC** — [torsova.com](https://torsova.com)

Ironclad is part of Torsova's suite of legacy modernization tools including transpilers for COBOL (Rust and C++17), HLASM, JCL, DFSORT, PL/I, REXX, Easytrieve, SAS, VB6, Stored Procedures, Crystal Reports, and Microsoft Access.

---

## License

Licensed under the [Apache License, Version 2.0](LICENSE).

The original CMS Medicare pricer programs are U.S. Government works in the public domain.

All modifications and additions — including the Rust transpiled programs, the parity validator, and the test suite — are Copyright 2025–2026 Michael R. Mull / Torsova LLC. See [NOTICE](NOTICE) for details.
