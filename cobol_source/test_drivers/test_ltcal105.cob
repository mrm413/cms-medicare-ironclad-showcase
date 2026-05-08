       IDENTIFICATION DIVISION.
       PROGRAM-ID. test_ltcal105.

      ******************************************************************
      * Auto-generated test driver via driver_bridge.py
      * Source program:  LTCAL105.cob
      * Called program:  LTCAL105
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  BILL-NEW-DATA.
           10  B-NPI10.
               15  B-NPI8             PIC X(08).
               15  B-NPI-FILLER       PIC X(02).
           10  B-PROVIDER-NO          PIC X(06).
           10  B-PATIENT-STATUS       PIC X(02).
           10  B-DRG-CODE             PIC 9(03).
           10  B-LOS                  PIC 9(03).
           10  B-COV-DAYS             PIC 9(03).
           10  B-LTR-DAYS             PIC 9(02).
           10  B-DISCHARGE-DATE.
               15  B-DISCHG-CC        PIC 9(02).
               15  B-DISCHG-YY        PIC 9(02).
               15  B-DISCHG-MM        PIC 9(02).
               15  B-DISCHG-DD        PIC 9(02).
           10  B-COV-CHARGES          PIC 9(07)V9(02).
           10  B-SPEC-PAY-IND         PIC X(01).
           10  FILLER                 PIC X(13).




       01  PPS-DATA-ALL.
           05  PPS-RTC                       PIC 9(02).
           05  PPS-CHRG-THRESHOLD            PIC 9(07)V9(02).
           05  PPS-DATA.
               10  PPS-MSA                   PIC X(04).
               10  PPS-WAGE-INDEX            PIC 9(02)V9(04).
               10  PPS-AVG-LOS               PIC 9(02)V9(01).
               10  PPS-RELATIVE-WGT          PIC 9(01)V9(04).
               10  PPS-OUTLIER-PAY-AMT       PIC 9(07)V9(02).
               10  PPS-LOS                   PIC 9(03).
               10  PPS-DRG-ADJ-PAY-AMT       PIC 9(07)V9(02).
               10  PPS-FED-PAY-AMT           PIC 9(07)V9(02).
               10  PPS-FINAL-PAY-AMT         PIC 9(07)V9(02).
               10  PPS-FAC-COSTS             PIC 9(07)V9(02).
               10  PPS-NEW-FAC-SPEC-RATE     PIC 9(07)V9(02).
               10  PPS-OUTLIER-THRESHOLD     PIC 9(07)V9(02).
               10  PPS-SUBM-DRG-CODE         PIC X(03).
               10  PPS-CALC-VERS-CD          PIC X(05).
               10  PPS-REG-DAYS-USED         PIC 9(03).
               10  PPS-LTR-DAYS-USED         PIC 9(03).
               10  PPS-BLEND-YEAR            PIC 9(01).
               10  PPS-COLA                  PIC 9(01)V9(03).
               10  FILLER                    PIC X(04).
           05  PPS-OTHER-DATA.
               10  PPS-NAT-LABOR-PCT         PIC 9(01)V9(05).
               10  PPS-NAT-NONLABOR-PCT      PIC 9(01)V9(05).
               10  PPS-STD-FED-RATE          PIC 9(05)V9(02).
               10  PPS-BDGT-NEUT-RATE        PIC 9(01)V9(03).
               10  FILLER                    PIC X(16).
           05  PPS-PC-DATA.
               10  PPS-COT-IND               PIC X(01).
               10  H-PC-IND                  PIC X(02).
                     88  PC-PRICER               VALUE 'PC'.
               10  FILLER                    PIC X(18).

       01 PPS-CBSA                           PIC X(05).


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
               05  P-NEW-SPECIAL-PAY-IND         PIC X(01).
               05  FILLER                        PIC X(01).
               05  P-NEW-GEO-LOC-CBSAX           PIC X(05) JUST RIGHT.
               05  P-NEW-GEO-LOC-CBSA9 REDEFINES
                             P-NEW-GEO-LOC-CBSAX PIC 9(05).
               05  P-NEW-GEO-LOC-CBSA-AST REDEFINES
                             P-NEW-GEO-LOC-CBSAX.
                   10 P-NEW-GEO-LOC-CBSA-1ST     PIC X.
                   10 P-NEW-GEO-LOC-CBSA-2ND     PIC X.
                   10 P-NEW-GEO-LOC-CBSA-3RD     PIC X.
                   10 P-NEW-GEO-LOC-CBSA-4TH     PIC X.
                   10 P-NEW-GEO-LOC-CBSA-5TH     PIC X.
               05  FILLER                        PIC X(10).
               05  P-NEW-SPECIAL-WAGE-INDEX      PIC 9(02)V9(04).
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


       01  WAGE-NEW-INDEX-RECORD.
           05  W-CBSA                        PIC X(5).
           05  W-EFF-DATE                    PIC X(8).
           05  W-WAGE-INDEX1                 PIC S9(02)V9(04).
           05  W-WAGE-INDEX2                 PIC S9(02)V9(04).
           05  W-WAGE-INDEX3                 PIC S9(02)V9(04).


       01  WAGE-NEW-IPPS-INDEX-RECORD.
           05  W-CBSA-IPPS.
               10 CBSA-IPPS-123              PIC X(3).
               10 CBSA-IPPS-45               PIC X(2).
           05  W-CBSA-IPPS-SIZE              PIC X.
               88  LARGE-URBAN       VALUE 'L'.
               88  OTHER-URBAN       VALUE 'O'.
               88  ALL-RURAL         VALUE 'R'.
           05  W-CBSA-IPPS-EFF-DATE          PIC X(8).
           05  FILLER                        PIC X.
           05  W-IPPS-WAGE-INDEX             PIC S9(02)V9(04).
           05  W-IPPS-PR-WAGE-INDEX          PIC S9(02)V9(04).




       PROCEDURE DIVISION.
       MAIN-LOGIC.
           INITIALIZE BILL-NEW-DATA
           INITIALIZE PPS-DATA-ALL
           INITIALIZE PPS-CBSA
           INITIALIZE PRICER-OPT-VERS-SW
           INITIALIZE PROV-NEW-HOLD
           INITIALIZE WAGE-NEW-INDEX-RECORD
           INITIALIZE WAGE-NEW-IPPS-INDEX-RECORD

           MOVE '12345678'     TO B-NPI8
           MOVE '12'           TO B-NPI-FILLER
           MOVE '123456'       TO B-PROVIDER-NO
           MOVE '01'           TO B-PATIENT-STATUS
           MOVE 207            TO B-DRG-CODE
           MOVE 045            TO B-LOS
           MOVE 045            TO B-COV-DAYS
           MOVE 00             TO B-LTR-DAYS
           MOVE 20             TO B-DISCHG-CC
           MOVE 20             TO B-DISCHG-YY
           MOVE 06             TO B-DISCHG-MM
           MOVE 15             TO B-DISCHG-DD
           MOVE 0150000.00     TO B-COV-CHARGES
           MOVE ' '            TO B-SPEC-PAY-IND

           CALL 'LTCAL105' USING BILL-NEW-DATA
                                PPS-DATA-ALL
                                PPS-CBSA
                                PRICER-OPT-VERS-SW
                                PROV-NEW-HOLD
                                WAGE-NEW-INDEX-RECORD
                                WAGE-NEW-IPPS-INDEX-RECORD

           DISPLAY 'PPS-RTC=[' PPS-RTC ']'
           DISPLAY 'PPS-MSA=[' PPS-MSA ']'
           DISPLAY 'PPS-FED-PAY-AMT=[' PPS-FED-PAY-AMT ']'
           DISPLAY 'PPS-FINAL-PAY-AMT=[' PPS-FINAL-PAY-AMT ']'
           DISPLAY 'PPS-CALC-VERS-CD=[' PPS-CALC-VERS-CD ']'
           STOP RUN.
