       IDENTIFICATION DIVISION.
       PROGRAM-ID. test_ipcal220.

      ******************************************************************
      * FY2022 IPF PPS Calculator parity test driver.
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

019100
       01  BILL-INPUT-DATA.
           05  BILL-IN-DATA.
               10  BILL-NPI-NUMBER.
                   15  BILL-NPI            PIC X(08).
                   15  BILL-NPI-FILLER     PIC X(02).
               10  BILL-PROVIDER-NO        PIC X(06).
               10  BILL-HIC-NO             PIC X(12).
               10  BILL-DISCHARGE-DATE.
                   15  BILL-D-CC           PIC 9(02).
                   15  BILL-D-YY           PIC 9(02).
                   15  BILL-D-MM           PIC 9(02).
                   15  BILL-D-DD           PIC 9(02).
               10  BILL-PATIENT-STATUS     PIC X(02).
               10  BILL-AGE                PIC 9(03).
               10  BILL-DRG                PIC 9(03).
               10  BILL-LOS                PIC 9(05).
               10  BILL-OUTL-OCCUR-IND     PIC X(01).
               10  BILL-SRC-OF-ADMISSION   PIC X(01).
               10  BILL-ECT-NO-OF-UNITS    PIC 9(03).
               10  BILL-CHARGES-CLAIMED    PIC 9(07)V9(02).
               10  BILL-DIAG-PROC-DATA.
                   15  BILL-OTHER-DIAG-DATA   OCCURS 25 TIMES.
                       20  BILL-DDXX-1ST     PIC X.
                       20  FILLER            PIC X(06).
                   15  BILL-OTHER-PROC-DATA PIC X(07)  OCCURS 25 TIMES.
               10  BILL-PRIOR-DAYS         PIC 9(03).
       01  IPF-DATA-VARIABLES.
               10  IPF-RTC                 PIC 9(02).
               10  IPF-MSA-CBSA            PIC X(05).
               10  IPF-MSA-CODE REDEFINES IPF-MSA-CBSA.
                   15  IPF-MSA             PIC X(04).
                   15  FILLER              PIC X.
               10  IPF-CBSA-CODE REDEFINES IPF-MSA-CBSA.
                   15  IPF-CBSA            PIC X(05).
               10  IPF-WAGE-INDEX          PIC 9(02)V9(04).
               10  IPF-LABOR-SHARE         PIC 9(01)V9(05).
               10  IPF-NLABOR-SHARE        PIC 9(01)V9(05).
               10  IPF-COLA                PIC 9(01)V9(03).
               10  IPF-STD-FACTOR          PIC 9(01)V9(05).
               10  IPF-COMORB-FACTOR       PIC 9(01)V9(05).
               10  IPF-AGE-ADJ             PIC 9(01)V9(02).
               10  IPF-DRG-FACTOR          PIC 9(01)V9(02).
               10  IPF-GEO-RURAL-ADJ       PIC 9(01)V9(02).
               10  IPF-EMERG-ADJ           PIC 9(01)V9(02).
               10  IPF-TEACH-ADJ           PIC 9(01)V9(02).
               10  IPF-FED-PPS-BLEND-IND   PIC X.
               10  IPF-CAL-VERSION         PIC X(05).
               10  IPF-CSTCHG-RATIO        PIC 9(01)V9(03).
               10  FILLER                  PIC X(08).
027900
       01  IPF-ADDITIONAL-VARIABLES.
           02  IPF-MF-VARIABLES.
               10  IPF-100PCT-STOPLOS-AMT      PIC 9(07)V9(02).
               10  IPF-TOT-PAYMENT             PIC 9(07)V9(02).
               10  IPF-FED-PAYMENT             PIC 9(07)V9(02).
               10  IPF-FAC-PAYMENT             PIC 9(07)V9(02).
               10  IPF-ECT-PAYMENT             PIC 9(07)V9(02).
               10  IPF-OUTLIER-PAYMENT         PIC 9(07)V9(02).
               10  IPF-OUTL-COST               PIC 9(07)V9(02).
               10  IPF-OUTL-ADJ-COST           PIC 9(07)V9(02).
               10  IPF-OUTL-PER-DIEM-AMT       PIC 9(07)V9(02).
               10  IPF-OUTL-THRES-AMT          PIC 9(07)V9(02).
               10  IPF-OUTL-THRES-ADJ-AMT      PIC 9(07)V9(02).
               10  IPF-ADJUSTED-PER-DIEM-AMT   PIC 9(07)V9(02).
               10  IPF-WAGE-ADJ-AMT            PIC 9(07)V9(02).
               10  IPF-LABOR-BASE-AMT          PIC 9(07)V9(05).
               10  IPF-NLABOR-BASE-AMT         PIC 9(07)V9(05).
               10  IPF-OUTL-LABOR-BASE-AMT     PIC 9(07)V9(05).
               10  IPF-OUTL-NLABOR-BASE-AMT    PIC 9(07)V9(05).
               10  IPF-BUDGNUT-RATE-AMT        PIC 9(05)V9(02).
               10  IPF-ECT-RATE-AMT            PIC 9(05)V9(02).
               10  IPF-TEACH-PAYMENT           PIC 9(07)V9(02).
               10  FILLER                      PIC X(01).
            02 IPF-PC-VARIABLES.
               10  IPF-PC-DATA                 PIC X(44).
030800
       01  PRICER-OPT-VERS-SW.
           02  PRICER-OPTION-SW          PIC X(01).
               88  VARIABLES                  VALUE 'S'.
               88  PROV-RECORD-PASSED         VALUE 'P'.
               88  ALL-TABLES-PASSED          VALUE 'B'.
               88  PC-PRICER                  VALUE 'C'.
           02  IPF-VERSIONS.
               10  IPDRV-VERSION         PIC X(05).
031700
       01  PROV-NEW-HOLD.
           02  PROV-NEWREC-HOLD1.
               05  P-NEW-NPI10.
                   10  P-NEW-NPI8             PIC X(08).
                   10  P-NEW-NPI-FILLER       PIC X(02).
               05  P-NEW-PROVIDER-NO.
                   88  P-NEW-DSH-ADJ-PROVIDERS
                                   VALUE '180049' '190044' '190144'
                                         '190191' '330047' '340085'
                                         '370016' '370149' '420043'.
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
                   88  P-N-SOLE-COMMUNITY-PROV    VALUE '01' '11'.
                   88  P-N-REFERRAL-CENTER        VALUE '07' '11'
                                                        '15' '17'
                                                        '22'.
                   88  P-N-INDIAN-HEALTH-SERVICE  VALUE '08'.
                   88  P-N-REDESIGNATED-RURAL-YR1 VALUE '09'.
                   88  P-N-REDESIGNATED-RURAL-YR2 VALUE '10'.
                   88  P-N-SOLE-COM-REF-CENT      VALUE '11'.
                   88  P-N-MDH-REBASED-FY90       VALUE '14' '15'.
                   88  P-N-MDH-RRC-REBASED-FY90   VALUE '15'.
                   88  P-N-SCH-REBASED-FY90       VALUE '16' '17'.
                   88  P-N-SCH-RRC-REBASED-FY90   VALUE '17'.
                   88  P-N-MEDICAL-ASSIST-FACIL   VALUE '18'.
                   88  P-N-EACH                   VALUE '21' '22'.
                   88  P-N-EACH-REFERRAL-CENTER   VALUE '22'.
                   88  P-N-NHCMQ-II-SNF           VALUE '32'.
                   88  P-N-NHCMQ-III-SNF          VALUE '33'.
               05  P-NEW-CURRENT-CENSUS-DIV   PIC 9(01).
                   88  P-N-NEW-ENGLAND            VALUE  1.
                   88  P-N-MIDDLE-ATLANTIC        VALUE  2.
                   88  P-N-SOUTH-ATLANTIC         VALUE  3.
                   88  P-N-EAST-NORTH-CENTRAL     VALUE  4.
                   88  P-N-EAST-SOUTH-CENTRAL     VALUE  5.
                   88  P-N-WEST-NORTH-CENTRAL     VALUE  6.
                   88  P-N-WEST-SOUTH-CENTRAL     VALUE  7.
                   88  P-N-MOUNTAIN               VALUE  8.
                   88  P-N-PACIFIC                VALUE  9.
               05  P-NEW-CURRENT-DIV   REDEFINES
                          P-NEW-CURRENT-CENSUS-DIV   PIC 9(01).
                   88  P-N-VALID-CENSUS-DIV    VALUE 1 THRU 9.
               05  P-NEW-MSA-DATA.
                   10  P-NEW-CHG-CODE-INDEX       PIC X.
                   10  P-NEW-GEO-LOC-MSAX         PIC X(04) JUST RIGHT.
                   10  P-NEW-GEO-LOC-MSA9   REDEFINES
                                   P-NEW-GEO-LOC-MSAX  PIC 9(04).
                   10  P-NEW-GEO REDEFINES
                                       P-NEW-GEO-LOC-MSAX.
                       15  P-NEW-GEO-RURAL-1ST.
                           20  P-NEW-GEO-RURAL  PIC XX.
                               88  P-NEW-GEO-MSAX-RURAL VALUE '  '.
                       15  P-NEW-GEO-RURAL-2ND        PIC XX.
                   10  P-NEW-WAGE-INDEX-LOC-MSA   PIC X(04) JUST RIGHT.
                   10  P-NEW-STAND-AMT-LOC-MSA    PIC X(04) JUST RIGHT.
                   10  P-NEW-STAND-AMT-LOC-MSA9
             REDEFINES P-NEW-STAND-AMT-LOC-MSA.
                       15  P-NEW-RURAL-1ST.
                           20  P-NEW-STAND-RURAL  PIC XX.
                               88  P-NEW-STD-RURAL-CHECK VALUE '  '.
                       15  P-NEW-RURAL-2ND        PIC XX.
               05  P-NEW-SOL-COM-DEP-HOSP-YR PIC XX.
                       88  P-NEW-SCH-YRBLANK    VALUE   '  '.
                       88  P-NEW-SCH-YR82       VALUE   '82'.
                       88  P-NEW-SCH-YR87       VALUE   '87'.
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
                   10  P-NEW-CBSA-GEO-LOCX REDEFINES
                       P-NEW-CBSA-GEO-LOC.
                       15  P-NEW-CBSA-GEO-RURAL-1ST.
                           20  P-NEW-CBSA-GEO-RURAL  PIC XXX.
                               88  P-NEW-CBSA-GEO-RURAL-CHECK
                                   VALUE '   '.
                       15  P-NEW-CBSA-GEO-RURAL-2ND PIC XX.
                   10  P-NEW-CBSA-RECLASS-LOC    PIC X(05) JUST RIGHT.
                   10  P-NEW-CBSA-STAND-AMT-LOC  PIC X(05) JUST RIGHT.
                   10  P-NEW-CBSA-SPEC-WI        PIC 9(02)V9(04).
                   10  P-NEW-CBSA-SPEC-WI-N  REDEFINES
                       P-NEW-CBSA-SPEC-WI        PIC 9(06).
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
                   15  P-VAL-BASED-PURCH-SCORE    PIC 9V999.
               05  P-NEW-SUPPLEMENTAL-WI.
                   10  P-NEW-SUPP-WI-IND         PIC X.
                       88 SUPP-WI-PRIOR-YEAR     VALUE '1'.
                   10  P-NEW-SUPP-WI             PIC 9(2)V9(04).
               05  FILLER                        PIC X(11).
047400
       01  WAGE-INDEX-RECORD.
           05  W-CBSA              PIC 9(5).
           05  W-SIZE              PIC X(01).
               88  LARGE-URBAN       VALUE 'L'.
               88  OTHER-URBAN       VALUE 'O'.
               88  ALL-RURAL         VALUE 'R'.
           05  W-CBSA-EFF-DATE     PIC 9(8).
           05  FILLER              PIC X.
           05  W-CBSA-WAGE-INDEX   PIC S9(02)V9(04).
           05  FILLER              PIC S9(02)V9(04).
      *    EJECT
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           INITIALIZE BILL-INPUT-DATA
           INITIALIZE IPF-DATA-VARIABLES
           INITIALIZE IPF-ADDITIONAL-VARIABLES
           INITIALIZE PRICER-OPT-VERS-SW
           INITIALIZE PROV-NEW-HOLD
           INITIALIZE WAGE-INDEX-RECORD

           CALL 'IPCAL220' USING BILL-INPUT-DATA
                                  IPF-DATA-VARIABLES
                                  IPF-ADDITIONAL-VARIABLES
                                  PRICER-OPT-VERS-SW
                                  PROV-NEW-HOLD
                                  WAGE-INDEX-RECORD

           DISPLAY 'IPF-RTC=[' IPF-RTC ']'
           DISPLAY 'IPF-MSA-CBSA=[' IPF-MSA-CBSA ']'
           DISPLAY 'IPF-WAGE-INDEX=' IPF-WAGE-INDEX
           DISPLAY 'IPF-CAL-VERSION=[' IPF-CAL-VERSION ']'
           DISPLAY 'IPF-TOT-PAYMENT=' IPF-TOT-PAYMENT
           STOP RUN.
