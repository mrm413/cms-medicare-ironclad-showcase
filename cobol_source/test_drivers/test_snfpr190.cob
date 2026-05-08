       IDENTIFICATION DIVISION.
       PROGRAM-ID. test_snfpr190.

      ******************************************************************
      * FY2020 SNF PPS Pricer (SNFPR190) parity test driver.
      * Tests RUG-IV-based payment calculation (pre-PDPM).
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  SNF-DATA.
           10  SNF-MSA               PIC XXXX.
           10  SNF-CBSA              PIC XXXXX.
           10  SNF-SPEC-WI-IND       PIC X.
           10  SNF-SPEC-WI           PIC 9(02)V9(04).
           10  SNF-RUG-CODE.
               15  SNF-RUG           PIC XXX.
               15  FILLER            PIC XX.

           10  SNF-FROM-DATE.
               15  SNF-FROM-CC        PIC XX.
               15  SNF-FROM-YY        PIC XX.
               15  SNF-FROM-MM        PIC XX.
               15  SNF-FROM-DD        PIC XX.

           10  SNF-THRU-DATE.
               15  SNF-THRU-CC       PIC XX.
               15  SNF-THRU-YY       PIC XX.
               15  SNF-THRU-MM       PIC XX.
               15  SNF-THRU-DD       PIC XX.

           10  SNF-FED-BLEND         PIC X.
               88  SNF-ALL-FACILITY      VALUE '0'.
               88  SNF-FED-BLEND-2575PER VALUE '1'.
               88  SNF-FED-BLEND-5050PER VALUE '2'.
               88  SNF-FED-BLEND-7525PER VALUE '3'.
               88  SNF-FED-BLEND-100PER  VALUE '4'.
           10  SNF-FACILITY-RATE     PIC 9(05)V99.
           10  SNF-DIAGNOSIS-CODES.
               15  SNF-PRIN-DIAG-CODE    PIC X(07).
               15  SNF-OTHER-DIAG-CODE2  PIC X(07).
               15  SNF-OTHER-DIAG-CODE3  PIC X(07).
               15  SNF-OTHER-DIAG-CODE4  PIC X(07).
               15  SNF-OTHER-DIAG-CODE5  PIC X(07).
               15  SNF-OTHER-DIAG-CODE6  PIC X(07).
               15  SNF-OTHER-DIAG-CODE7  PIC X(07).
               15  SNF-OTHER-DIAG-CODE8  PIC X(07).
               15  SNF-OTHER-DIAG-CODE9  PIC X(07).
               15  SNF-OTHER-DIAG-CODE10 PIC X(07).
               15  SNF-OTHER-DIAG-CODE11 PIC X(07).
               15  SNF-OTHER-DIAG-CODE12 PIC X(07).
               15  SNF-OTHER-DIAG-CODE13 PIC X(07).
               15  SNF-OTHER-DIAG-CODE14 PIC X(07).
               15  SNF-OTHER-DIAG-CODE15 PIC X(07).
               15  SNF-OTHER-DIAG-CODE16 PIC X(07).
               15  SNF-OTHER-DIAG-CODE17 PIC X(07).
               15  SNF-OTHER-DIAG-CODE18 PIC X(07).
               15  SNF-OTHER-DIAG-CODE19 PIC X(07).
               15  SNF-OTHER-DIAG-CODE20 PIC X(07).
               15  SNF-OTHER-DIAG-CODE21 PIC X(07).
               15  SNF-OTHER-DIAG-CODE22 PIC X(07).
               15  SNF-OTHER-DIAG-CODE23 PIC X(07).
               15  SNF-OTHER-DIAG-CODE24 PIC X(07).
               15  SNF-OTHER-DIAG-CODE25 PIC X(07).
           10  SNF-RATE-RTC.
               15  SNF-PAYMENT-RATE      PIC 9(06)V99.
               15  SNF-RTC               PIC XX.
           10  VBP-DATA.
               15  VBP-MULTIPLIER        PIC S9V9(11).
               15  VBP-PAY-DIFF          PIC S9(06)V9(02).
           10  FILLER                      PIC X(50).



       01  HOLD-VARIABLES.
           02  HOLD-VAR-DATA.
               05  FACTOR                         PIC 9.
               05  NUR-INDEX                      PIC 9V99.
               05  THR-INDEX                      PIC 9V99.
               05  AREA-WAGE-INDEX                PIC 9(01)V9(04).
               05  IP-RATE                        PIC 9(03)V9(02).
               05  GS-RATE                        PIC 9(02)V9(02).
               05  TH-RATE                        PIC 9(02)V9(02).
               05  REHAB-RATE                     PIC 9(03)V9(02).
               05  NURSING-COMPONENT              PIC 999V99.
               05  THERAPY-COMPONENT              PIC 999V99.
               05  NCM-THR-COMPONENT              PIC 999V99.
               05  NCM-COMPONENT                  PIC 999V99.
               05  PAYMENT-RATE-ADJ               PIC 9(06)V99.
               05  FED-PAYMENT-PORTION            PIC 9(06)V99.
           02  SNFPR-VERSION                      PIC X(09).

       01  MSA-WAGE-INDEX-RECORD.
           02  HOLD-MSA-WIR-DATA.
               05  MSA-WIR-MSA             PIC X(04).
               05  MSA-WIR-EFFDATE         PIC X(08).
               05  MSA-WIR-AREA-WAGEIND    PIC 9(02)V9(04).

       01  CBSA-WAGE-INDEX-RECORD.
           02  HOLD-CBSA-WIR-DATA.
               05  CBSA-WIR-CBSA            PIC X(05).
               05  CBSA-WIR-EFFDATE         PIC X(08).
               05  CBSA-WIR-AREA-WAGEIND    PIC 9(02)V9(04).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           INITIALIZE SNF-DATA

      *    FY2020 SNF claim — pre-October 2019 RUG-IV system
           MOVE '0000'      TO SNF-MSA
           MOVE '11260'     TO SNF-CBSA
           MOVE ' '         TO SNF-SPEC-WI-IND
           MOVE 'AAA'       TO SNF-RUG
           MOVE '20'        TO SNF-FROM-CC
           MOVE '19'        TO SNF-FROM-YY
           MOVE '06'        TO SNF-FROM-MM
           MOVE '01'        TO SNF-FROM-DD
           MOVE '20'        TO SNF-THRU-CC
           MOVE '19'        TO SNF-THRU-YY
           MOVE '06'        TO SNF-THRU-MM
           MOVE '15'        TO SNF-THRU-DD
           MOVE '4'         TO SNF-FED-BLEND

           CALL 'SNFPR190' USING SNF-DATA

           DISPLAY 'SNF-RTC=[' SNF-RTC ']'
           DISPLAY 'SNF-PAYMENT-RATE=' SNF-PAYMENT-RATE
           STOP RUN.
