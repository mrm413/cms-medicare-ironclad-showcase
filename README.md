# Ironclad: CMS Medicare Payment System — COBOL-to-Rust Transpilation

**55 CMS Medicare pricer programs | 92,535 lines of production COBOL | 169,475 lines of generated Rust | 100% compile | Zero external dependencies**

This repository contains the **output** of the Ironclad transpilation system applied to real-world CMS (Centers for Medicare & Medicaid Services) payment system COBOL. Every `.rs` file here was generated automatically from production Medicare pricer source code.

Ironclad is a proprietary transpilation engine built by [Torsova LLC](https://lazarus-systems.com). The source code for Ironclad is not included in this repository.

---

## What Is This?

CMS Medicare pricers are the COBOL programs that calculate payment rates for Medicare services across the United States. These are not toy programs — they are production mainframe code with packed decimals, REDEFINES overlays, OCCURS DEPENDING ON tables, COMP-3 arithmetic, and multi-level copybook hierarchies.

Ironclad transpiled all 55 programs to Rust. This repository is the proof.

| Metric | Value |
|--------|-------|
| COBOL programs processed | 55 |
| Rust programs generated | 55 (100%) |
| Rust programs that compile | 55 (100%) |
| Total COBOL lines | 92,535 |
| Total generated Rust lines | 169,475 |
| Runtime library lines | 5,740 (14 modules) |
| External dependencies | 0 |
| AI/LLM in the loop | None |

### Pricer Systems

| System | Programs | Description |
|--------|----------|-------------|
| ESRD (End-Stage Renal Disease) | 21 | Dialysis facility payment rates |
| LTCH (Long-Term Care Hospital) | 30 | Long-term acute care DRG pricing |
| Hospice | 2 | Hospice per-diem payment rates |
| SNF (Skilled Nursing Facility) | 2 | PDPM nursing facility payment rates |

---

## Why This Matters

These are not test programs. They are the actual COBOL that runs on CMS mainframes to determine how much Medicare pays hospitals, dialysis centers, hospice providers, and skilled nursing facilities.

The COBOL has characteristics that break most transpilers:

- **REDEFINES chains** — fields redefining fields that redefine other fields (up to 3 levels deep)
- **COMP-3 packed decimal** — mainframe BCD arithmetic (`PIC S9(11)V99 COMP-3`)
- **80-column fixed format** — sequence numbers in cols 1-6, indicator in col 7, code in cols 8-72
- **Deep copybook hierarchies** — 69 copybooks with cross-program shared data structures
- **Table lookups** — OCCURS with DEPENDING ON, binary search, multi-dimensional indexing
- **Mixed numeric types** — COMP, COMP-3, DISPLAY numeric, with implicit conversion between all of them

Ironclad handles all of this deterministically.

---

## The Four-Stage Pipeline

```
  COBOL Source (.cob)
      |
      v
  [1. Parser ]          DATA DIVISION  -> typed field map
      |                  PROCEDURE DIV  -> verb-level AST
      |                  COPY/REPLACE   -> expanded inline
      v
  Typed COBOL IR          structs, enums, decimals, file descriptors
      |
  [2. Rustifier ]       PIC -> Rust types, PERFORM -> loops,
      |                  EVALUATE -> match, READ/WRITE -> Result<T,E>
      v
  Rust AST                real structs, real enums, real error handling
      |
  [3. Emitter ]         formatted .rs output
      |
      v
  Idiomatic Rust (.rs)    cargo build
      |
  [4. Validator ]        same inputs -> same outputs
      v
  Equivalence Report      PASS/FAIL per test vector
```

Every stage is deterministic. Same COBOL input always produces the same Rust output. No randomness, no LLM, no heuristics.

---

## Repository Structure

```
cms-medicare-ironclad-showcase/
  README.md                          # This file
  Cargo.toml                         # Workspace manifest
  cobol-runtime/
    src/
      lib.rs                         # Core types: FixedString, Decimal, PackedDecimal
      decimal.rs                     # Fixed-point exact arithmetic
      packed_decimal.rs              # COMP-3 packed decimal arithmetic
      fixed_string.rs                # FixedString<N> — PIC X(N) equivalent
      cobol_file.rs                  # CobolFile, sequential/indexed I/O
      file_status.rs                 # FileStatus codes (00, 10, 35, etc.)
      string_ops.rs                  # STRING, UNSTRING, INSPECT operations
      ebcdic.rs                      # EBCDIC/ASCII conversion tables
      chrono_shim.rs                 # Date/time functions (ACCEPT FROM DATE)
      report_writer.rs               # Report Writer stubs
      cics.rs                        # CICS runtime stubs
      dli.rs                         # DL/I database stubs
      edited_numeric.rs              # Edited numeric display
      sql.rs                         # Embedded SQL stubs
  cobol_source/                      # All 55 original COBOL programs
  rust_output/                       # All 55 transpiled Rust programs
  src/bin/                           # Rust binaries (same as rust_output/)
  samples/                           # 4 curated before/after pairs
    snfdr211/                        # SNF Driver — wage index + CBSA lookups
    escal212/                        # ESRD Calculation — dialysis rate math
    esdrv212/                        # ESRD Driver — file I/O + CALL linkage
    hospr210/                        # Hospice Pricer — per-diem payment rates
```

---

## Type Mapping

| COBOL | Rust | Notes |
|-------|------|-------|
| `PIC X(N)` | `FixedString<N>` | Space-padded, EBCDIC-safe |
| `PIC 9(N)` | `u32` / `u64` | Display numeric |
| `PIC S9(N)` | `i32` / `i64` | Signed display |
| `PIC S9(N)V9(M)` | `Decimal` | Fixed-point exact arithmetic |
| `PIC S9(N) COMP` | `i16` / `i32` / `i64` | Binary native |
| `PIC S9(N) COMP-3` | `PackedDecimal<N>` | BCD packed decimal |
| `88-level` | enum variant | Condition names |
| `OCCURS N TIMES` | `[T; N]` | Fixed array |
| `OCCURS DEPENDING ON` | `Vec<T>` | Variable length |
| `REDEFINES` | struct overlay | Type-safe reinterpretation |
| `FD file-name` | `CobolFile` + `BufReader`/`Writer` | File descriptor |

---

## Looking at the Output

### COBOL Input (SNFDR211 — SNF Driver, excerpt)

```cobol
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.          SNFDR211.
000300*AUTHOR.                 CMS.
000800*REMARKS. (CENTERS FOR MEDICARE AND MEDICAID SERVICES)
000900***         - NATIONAL SNF PRICER EFFECTIVE OCT 1, 2020
001000***         - SNF PRICER REFERS TO A PROGRAM WHICH WILL
001100***           CALCULATE THE MEDICARE RATE UPON WHICH THE
001200***           PDPM SNF PPS PAYMENT IS MADE.
```

### Rust Output (SNFDR211, excerpt)

```rust
// Generated by Ironclad — Deterministic COBOL-to-Rust Transpiler

use cobol_runtime::FixedString;
use cobol_runtime::Decimal;
use cobol_runtime::PackedDecimal;
use cobol_runtime::FileStatus;
use cobol_runtime::CobolFile;

#[derive(Debug, Clone, Default, PartialEq)]
pub struct CbsaWageIndexRecord {
    /// CBSA-WIR-CBSA
    pub cbsa_wir_cbsa: FixedString<5>,
    /// CBSA-WIR-EFFDATE
    pub cbsa_wir_effdate: FixedString<8>,
    /// CBSA-WIR-AREA-WAGEIND
    pub cbsa_wir_area_wageind: Decimal /* 2,4 Unsigned */,
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

Every COBOL data structure becomes a Rust struct with doc comments preserving the original field names. Every `PIC` clause maps to the correct Rust type. Every paragraph becomes a function. The Rust compiler enforces safety on every line.

---

## Compile Results

All 55 programs compile with `cargo build`. The generated Rust passes the borrow checker, type checker, and lifetime analysis — producing 55 native executables from 92,535 lines of mainframe COBOL.

---

## The Runtime Library

The `cobol-runtime` crate is pure Rust with **zero external dependencies**. It provides the types and operations that COBOL programs need at runtime:

- **`FixedString<N>`** — Fixed-length, space-padded strings matching `PIC X(N)` semantics. No heap allocation for strings that fit the fixed buffer.
- **`Decimal`** — Exact fixed-point arithmetic so `0.1 + 0.2 == 0.3`. Financial math that matches COBOL penny-for-penny.
- **`PackedDecimal<N>`** — COMP-3 Binary Coded Decimal with the exact byte layout of mainframe packed fields.
- **`CobolFile`** — Sequential, indexed, and relative file I/O with `FileStatus` codes matching the COBOL standard.
- **`EBCDIC`** — Full EBCDIC-to-ASCII conversion tables for mainframe data migration.

5,740 lines of Rust across 14 modules. No `unsafe` blocks. No FFI. No C dependencies.

---

## What Makes This Different

1. **Real production code** — Not test programs. These are the actual CMS Medicare pricers that determine payment rates for millions of Medicare claims.
2. **Enterprise COBOL complexity** — REDEFINES chains, COMP-3 packed decimals, 69 copybooks, multi-program CALL linkage. The hard stuff that toy transpilers skip.
3. **Deterministic** — Same COBOL input always produces the same Rust output. No randomness, no LLM, no heuristic guessing.
4. **Direct COBOL-to-Rust** — No C or C++ intermediate stage. COBOL's typed data definitions map directly to Rust structs with doc comments preserving original field names.
5. **Zero dependencies** — The runtime library is pure Rust with no external crates, no FFI, no C bindings.
6. **Government-grade** — Audit trail, reproducible builds, NIST-friendly provenance chain.

---

## Also Available: Lazarus C++17

All 55 CMS Medicare programs also compile through the [Lazarus](https://github.com/mrm413/cms-medicare-lazarus-showcase) pipeline to hardened C++17 with 100% compile success. Ironclad (Rust) and Lazarus (C++17) are complementary — same COBOL input, different target languages, different tradeoffs.

---

## Related Showcases

- [CMS Medicare — Lazarus C++17](https://github.com/mrm413/cms-medicare-lazarus-showcase) -- 55 CMS Medicare pricer programs transpiled to hardened C++17 (100%)
- [Lazarus COBOL Showcase](https://github.com/mrm413/lazarus-cobol-showcase) -- 1,607 GnuCOBOL test programs transpiled to hardened C++17 (100%)
- [Lazarus CardDemo Showcase](https://github.com/mrm413/lazarus-carddemo-showcase) -- 44 AWS CardDemo CICS/COBOL programs transpiled to C++17 (100%)

---

## Related Showcases

- [CMS Medicare — Lazarus C++17](https://github.com/mrm413/cms-medicare-lazarus-showcase) -- 55 CMS Medicare pricer programs transpiled to hardened C++17 (100%)
- [GnuCOBOL Test Suite — Lazarus C++17](https://github.com/mrm413/lazarus-cobol-showcase) -- 1,607 GnuCOBOL 3.2 test programs transpiled to hardened C++17 (100%)
- [Lazarus CardDemo Showcase](https://github.com/mrm413/lazarus-carddemo-showcase) -- 44 AWS CardDemo CICS/COBOL programs transpiled to C++17 (100%)

---

## Built By

**Torsova LLC** — [lazarus-systems.com](https://lazarus-systems.com)

Ironclad is part of a suite of legacy modernization tools including transpilers for COBOL (C++17 and Rust), VB6, Stored Procedures, Crystal Reports, SAS, and Microsoft Access.

---

## License

Licensed under the [Apache License, Version 2.0](LICENSE).

The original CMS Medicare pricer programs are U.S. Government works in the public domain.

All modifications and additions -- including the Rust transpiled programs, build system, and test suite -- are Copyright 2025 Michael R. Mull / Lazarus Systems. See [NOTICE](NOTICE) for details.
