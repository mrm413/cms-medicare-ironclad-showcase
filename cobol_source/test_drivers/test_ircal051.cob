       IDENTIFICATION DIVISION.
       PROGRAM-ID. test_ircal051.

      ******************************************************************
      * FY2020 IRF Calculator (IRCAL201) parity test driver.
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  BILL-NEW-DATA.
               10  B-NPI10.
                   15  B-NPI8             PIC X(08).
                   15  B-NPI-FILLER       PIC X(02).
               10  B-PROVIDER-NO          PIC X(06).
               10  B-PATIENT-STATUS       PIC X(02).
               10  B-CMG-CODE             PIC X(05).
               10  B-LOS                  PIC 9(03).
               10  B-COV-DAYS             PIC 9(03).
               10  B-LTR-DAYS             PIC 9(02).
               10  B-SPEC-PAY-IND         PIC X(01).
               10  B-DISCHARGE-DATE.
                   15  B-DISCHG-CC        PIC 9(02).
                   15  B-DISCHG-YY        PIC 9(02).
                   15  B-DISCHG-MM        PIC 9(02).
                   15  B-DISCHG-DD        PIC 9(02).
               10  B-COV-CHARGES          PIC 9(07)V9(02).
               10  FILLER                 PIC X(11).

       01  PPS-DATA-ALL.
           05  PPS-RTC                      PIC 9(02).
           05  PPS-DATA.
               10  PPS-MSA                  PIC X(04).
               10  PPS-WAGE-INDEX           PIC 9(02)V9(04).
               10  PPS-AVG-LOS              PIC 9(02).
               10  PPS-RELATIVE-WGT         PIC 9(01)V9(04).
               10  PPS-TOTAL-PAY-AMT        PIC 9(07)V9(02).
               10  PPS-FED-PAY-AMT          PIC 9(07)V9(02).
               10  PPS-FAC-SPEC-PAY-AMT     PIC 9(07)V9(02).
               10  PPS-OUTLIER-PAY-AMT      PIC 9(07)V9(02).
               10  PPS-LIP-PAY-AMT          PIC 9(07)V9(02).
               10  PPS-LIP-PCT              PIC 9(01)V9(04).
               10  PPS-LOS                  PIC 9(03).
               10  PPS-REG-DAYS-USED        PIC 9(03).
               10  PPS-LTR-DAYS-USED        PIC 9(03).
               10  PPS-TRANSFER-PCT         PIC 9(01)V9(04).
               10  PPS-FAC-SPEC-RT-PREBLEND PIC 9(05)V9(02).
               10  PPS-STANDARD-PAY-AMT     PIC 9(07)V9(02).
               10  PPS-FAC-COSTS            PIC 9(07)V9(02).
               10  PPS-OUTLIER-THRESHOLD    PIC 9(07)V9(02).
               10  PPS-CHG-OUTLIER-THRESHOLD PIC 9(07)V9(02).
               10  PPS-TOTAL-PENALTY-AMT    PIC 9(07)V9(02).
               10  PPS-FED-PENALTY-AMT      PIC 9(07)V9(02).
               10  PPS-LIP-PENALTY-AMT      PIC 9(07)V9(02).
               10  PPS-OUT-PENALTY-AMT      PIC 9(07)V9(02).
               10  PPS-SUBM-CMG-CODE        PIC X(05).
               10  PPS-CMG-CODE-REDEF REDEFINES PPS-SUBM-CMG-CODE.
                  15  PPS-CMG-ALPHA         PIC X(01).
                  15  PPS-CMG-NUMERIC.
                     20  PPS-CMG-RIC        PIC X(02).
                     20  FILLER             PIC X(02).
               10  PPS-PRICED-CMG-CODE      PIC X(05).
               10  PPS-CALC-VERS-CD         PIC X(05).
               10  PPS-CBSA                 PIC X(05).
               10  FILLER                   PIC X(08).
           05  PPS-OTHER-DATA.
               10  PPS-NAT-LABOR-PCT        PIC 9(01)V9(05).
               10  PPS-NAT-NONLABOR-PCT     PIC 9(01)V9(05).
               10  PPS-NAT-THRESHOLD-ADJ    PIC 9(05)V9(02).
               10  PPS-BDGT-NEUT-CONV-AMT   PIC 9(05)V9(02).
               10  PPS-FED-RATE-PCT         PIC 9(01)V9(04).
               10  PPS-FAC-RATE-PCT         PIC 9(01)V9(04).
               10  PPS-RURAL-ADJUSTMENT     PIC 9(01)V9(04).
               10  PPS-TEACH-PAY-AMT        PIC 9(07)V9(02).
               10  PPS-TEACH-PENALTY-AMT    PIC 9(07)V9(02).
               10  FILLER                   PIC X(02).
           05  PPS-PC-DATA.
               10  PPS-COT-IND              PIC X(01).
               10  FILLER                   PIC X(20).

       01  PRICER-OPT-VERS-SW.
           05  PRICER-OPTION-SW          PIC X(01).
               88  ALL-TABLES-PASSED          VALUE 'A'.
               88  PROV-RECORD-PASSED         VALUE 'P'.
           05  PPS-VERSIONS.
               10  PPDRV-VERSION         PIC X(05).

       01  PROV-NEW-HOLD.
           02  PROV-NEWREC-HOLD1.
               05  P-NEW-NPI10.
                   10  P-NEW-NPI8             PIC X(08).
                   10  P-NEW-NPI-FILLER       PIC X(02).
               05  P-NEW-PROVIDER-NO.
                   10  P-NEW-STATE            PIC 9(02).
                   10  FILLER                 PIC X(04).
               05  P-NEW-DATE-DATA.
                   10  P-NEW-EFF-DATE.
                       15  P-NEW-EFF-DT-CC    PIC 9(02).
                       15  P-NEW-EFF-DT-YY    PIC 9(02).
                       15  P-NEW-EFF-DT-MM    PIC 9(02).
                       15  P-NEW-EFF-DT-DD    PIC 9(02).
                   10  P-NEW-FY-BEGIN-DATE.
                       15  P-NEW-FY-BEG-DT-CC PIC 9(02).
                       15  P-NEW-FY-BEG-DT-YY PIC 9(02).
                       15  P-NEW-FY-BEG-DT-MM PIC 9(02).
                       15  P-NEW-FY-BEG-DT-DD PIC 9(02).
                   10  P-NEW-REPORT-DATE.
                       15  P-NEW-REPORT-DT-CC PIC 9(02).
                       15  P-NEW-REPORT-DT-YY PIC 9(02).
                       15  P-NEW-REPORT-DT-MM PIC 9(02).
                       15  P-NEW-REPORT-DT-DD PIC 9(02).
                   10  P-NEW-TERMINATION-DATE.
                       15  P-NEW-TERM-DT-CC   PIC 9(02).
                       15  P-NEW-TERM-DT-YY   PIC 9(02).
                       15  P-NEW-TERM-DT-MM   PIC 9(02).
                       15  P-NEW-TERM-DT-DD   PIC 9(02).
               05  P-NEW-WAIVER-CODE          PIC X(01).
                   88  P-NEW-WAIVER-STATE       VALUE 'Y'.
               05  P-NEW-INTER-NO             PIC 9(05).
               05  P-NEW-PROVIDER-TYPE        PIC X(02).
               05  P-NEW-CURRENT-CENSUS-DIV   PIC 9(01).
               05  P-NEW-CURRENT-DIV   REDEFINES
                          P-NEW-CURRENT-CENSUS-DIV   PIC 9(01).
               05  P-NEW-MSA-DATA.
                   10  P-NEW-CHG-CODE-INDEX       PIC X.
                   10  P-NEW-GEO-LOC-MSAX         PIC X(04) JUST RIGHT.
                   10  P-NEW-GEO-LOC-MSA9   REDEFINES
                                   P-NEW-GEO-LOC-MSAX  PIC 9(04).
                   10  P-NEW-WAGE-INDEX-LOC-MSA   PIC X(04) JUST RIGHT.
                   10  P-NEW-STAND-AMT-LOC-MSA    PIC X(04) JUST RIGHT.
                   10  P-NEW-STAND-AMT-LOC-MSA9
                       REDEFINES P-NEW-STAND-AMT-LOC-MSA.
                       15  P-NEW-RURAL-1ST.
                           20  P-NEW-STAND-RURAL  PIC XX.
                               88  P-NEW-STD-RURAL-CHECK VALUE '  '.
                       15  P-NEW-RURAL-2ND        PIC XX.
               05  P-NEW-SOL-COM-DEP-HOSP-YR PIC XX.
               05  P-NEW-LUGAR                    PIC X.
               05  P-NEW-TEMP-RELIEF-IND          PIC X.
               05  P-NEW-FED-PPS-BLEND-IND        PIC X.
               05  FILLER                         PIC X(05).
           02  PROV-NEWREC-HOLD2.
               05  P-NEW-VARIABLES.
                   10  P-NEW-FAC-SPEC-RATE     PIC  9(05)V9(02).
                   10  P-NEW-COLA              PIC  9(01)V9(03).
                   10  P-NEW-INTERN-RATIO      PIC  9(01)V9(04).
                   10  P-NEW-BED-SIZE          PIC  9(05).
                   10  P-NEW-OPER-CSTCHG-RATIO PIC  9(01)V9(03).
                   10  P-NEW-CMI               PIC  9(01)V9(04).
                   10  P-NEW-SSI-RATIO         PIC  V9(04).
                   10  P-NEW-MEDICAID-RATIO    PIC  V9(04).
                   10  P-NEW-PPS-BLEND-YR-IND  PIC  9(01).
                   10  P-NEW-PRUF-UPDTE-FACTOR PIC  9(01)V9(05).
                   10  P-NEW-DSH-PERCENT       PIC  V9(04).
                   10  P-NEW-FYE-DATE          PIC  X(08).
               05  P-NEW-CBSA-DATA.
                   10  P-NEW-CBSA-SPEC-PAY-IND   PIC X.
                   10  P-NEW-CBSA-HOSP-QUAL-IND  PIC X.
                   10  P-NEW-CBSA-GEO-LOC        PIC X(05) JUST RIGHT.
                   10  P-NEW-CBSA-RECLASS-LOC    PIC X(05) JUST RIGHT.
                   10  P-NEW-CBSA-STAND-AMT-LOC  PIC X(05) JUST RIGHT.
                   10  P-NEW-CBSA-STAND-AMT-LOC9
                       REDEFINES P-NEW-CBSA-STAND-AMT-LOC.
                       15  P-NEW-CBSA-RURAL-1ST.
                           20  P-NEW-CBSA-STAND-RURAL PIC 999.
                       15  P-NEW-CBSA-RURAL-2ND       PIC 99.
                   10  P-NEW-CBSA-SPEC-WAGE-INDEX     PIC 9(02)V9(04).
           02  PROV-NEWREC-HOLD3.
               05  P-NEW-PASS-AMT-DATA.
                   10  P-NEW-PASS-AMT-CAPITAL    PIC 9(04)V99.
                   10  P-NEW-PASS-AMT-DIR-MED-ED PIC 9(04)V99.
                   10  P-NEW-PASS-AMT-ORGAN-ACQ  PIC 9(04)V99.
                   10  P-NEW-PASS-AMT-PLUS-MISC  PIC 9(04)V99.
               05  P-NEW-CAPI-DATA.
                   15  P-NEW-CAPI-PPS-PAY-CODE   PIC X.
                   15  P-NEW-CAPI-HOSP-SPEC-RATE PIC 9(04)V99.
                   15  P-NEW-CAPI-OLD-HARM-RATE  PIC 9(04)V99.
                   15  P-NEW-CAPI-NEW-HARM-RATIO PIC 9(01)V9999.
                   15  P-NEW-CAPI-CSTCHG-RATIO   PIC 9V999.
                   15  P-NEW-CAPI-NEW-HOSP       PIC X.
                   15  P-NEW-CAPI-IME            PIC 9V9999.
                   15  P-NEW-CAPI-EXCEPTIONS     PIC 9(04)V99.
                   15  P-VAL-BASED-PURCH-SCORE   PIC 9V999.
               05  FILLER                        PIC X(18).
       01  WAGE-NEW-INDEX-RECORD-CBSA.
           05  W-NEW-CBSA                    PIC X(5).
           05  W-NEW-EFF-DATE-C              PIC X(8).
           05  W-NEW-WAGE-INDEX-C            PIC S9(02)V9(04).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           INITIALIZE BILL-NEW-DATA
           INITIALIZE PPS-DATA-ALL
           INITIALIZE PRICER-OPT-VERS-SW
           INITIALIZE PROV-NEW-HOLD
           INITIALIZE WAGE-NEW-INDEX-RECORD-CBSA

           CALL 'IRCAL051' USING BILL-NEW-DATA
                                  PPS-DATA-ALL
                                  PRICER-OPT-VERS-SW
                                  PROV-NEW-HOLD
                                  WAGE-NEW-INDEX-RECORD-CBSA

           DISPLAY 'PPS-RTC=[' PPS-RTC ']'
           STOP RUN.
