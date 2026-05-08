       IDENTIFICATION DIVISION.
       PROGRAM-ID. test_snfdr211.

      ******************************************************************
      * Runtime parity harness: drives SNFDR211 with a known input,
      * displays the output fields. Both GnuCOBOL and Ironclad-compiled
      * outputs should match byte-for-byte when transpiler logic is
      * sound. Uses the real SNF300P copybook so layout matches the
      * subprogram's expectations.
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *  --- SNF-DATA: real CMS SNF300P record ---
       COPY SNF300P.

      *  --- HOLD-VARIABLES: passed back from SNFPR ---
       01  HOLD-VARIABLES.
           02  HOLD-VAR-DATA.
               05  REGION-IND        PIC X.
               05  QRP-IND           PIC X.
               05  AIDS-ADD-ON-IND   PIC X.
               05  AREA-WAGE-INDEX   PIC 9(01)V9(04).
               05  IP-RATE           PIC 9(03)V9(02).
               05  GS-RATE           PIC 9(02)V9(02).
           02  SNFPR-VERSION         PIC X(09).

      *  --- CBSA-WI-TABLE: same shape as the SNFDR211 LINKAGE
      *      decl, including INDEXED BY so SEARCH codegen picks the
      *      first index (MA1) as the loop variable, not the fallback "I".
       01  CBSA-WI-TABLE.
           05  T-CBSA-DATA        OCCURS 8000
                                  INDEXED BY MA1 MA2 MA3.
               10  T-CBSA            PIC X(05).
               10  T-CBSA-EFFDATE    PIC X(08).
               10  T-CBSA-WAGEIND    PIC 9(02)V9(04).

       01  DUMMY-FLAG               PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
      *    Force Ironclad's companion discovery to also pull in
      *    SNFPR210 (transitive callee). Branch is unreachable at
      *    runtime — DUMMY-FLAG starts as 'N'.
           IF DUMMY-FLAG = 'Y'
               CALL 'SNFPR210' USING SNF-DATA HOLD-VARIABLES
           END-IF

      *    Initialize SNF-DATA / HOLD-VARIABLES to clean values.
           MOVE SPACES TO SNF-DATA
           MOVE SPACES TO HOLD-VARIABLES
           MOVE ZEROS  TO SNF-PAYMENT-RATE

      *    Test input: an urban CBSA, FY2021 dates, valid HIPPS-like.
           MOVE '0000'      TO SNF-MSA
           MOVE '11260'     TO SNF-CBSA
           MOVE ' '         TO SNF-SPEC-WI-IND
           MOVE 0           TO SNF-SPEC-WI
           MOVE 'AAA00'     TO SNF-HIPPS-CODE
           MOVE '20'        TO SNF-FROM-CC
           MOVE '20'        TO SNF-FROM-YY
           MOVE '10'        TO SNF-FROM-MM
           MOVE '15'        TO SNF-FROM-DD
           MOVE '20'        TO SNF-THRU-CC
           MOVE '20'        TO SNF-THRU-YY
           MOVE '10'        TO SNF-THRU-MM
           MOVE '20'        TO SNF-THRU-DD
           MOVE '0'         TO SNF-FED-BLEND
           MOVE 0           TO SNF-FACILITY-RATE
           MOVE ' '         TO SNF-SUPP-WI-IND
           MOVE 0           TO SNF-SUPP-WI

      *    Populate the wage-index table: one matching entry,
      *    rest blank so the program rejects them.
           MOVE '11260'     TO T-CBSA       (1)
           MOVE '20201001'  TO T-CBSA-EFFDATE (1)
           MOVE 1.0500      TO T-CBSA-WAGEIND (1)

      *    Call the driver.
           CALL 'SNFDR211' USING SNF-DATA
                                 HOLD-VARIABLES
                                 CBSA-WI-TABLE

      *    Display output fields — what we compare for parity.
           DISPLAY 'SNF-RTC=[' SNF-RTC ']'
           DISPLAY 'SNF-PAYMENT-RATE=' SNF-PAYMENT-RATE
           DISPLAY 'REGION-IND=[' REGION-IND ']'
           DISPLAY 'AREA-WAGE-INDEX=' AREA-WAGE-INDEX
           DISPLAY 'IP-RATE=' IP-RATE
           DISPLAY 'GS-RATE=' GS-RATE
           DISPLAY 'SNFPR-VERSION=[' SNFPR-VERSION ']'
           STOP RUN.
