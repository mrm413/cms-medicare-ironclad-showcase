       IDENTIFICATION DIVISION.
       PROGRAM-ID. test_snfdr211_v3.

      ******************************************************************
      * Third SNFDR211 parity test: high-cost facility, blend year 1,
      * with AIDS add-on, supplemental wage index. Tests yet another
      * code path through SNFDR211 + SNFPR210 (FY2021 SNF).
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SNF300P.

       01  HOLD-VARIABLES.
           02  HOLD-VAR-DATA.
               05  REGION-IND        PIC X.
               05  QRP-IND           PIC X.
               05  AIDS-ADD-ON-IND   PIC X.
               05  AREA-WAGE-INDEX   PIC 9(01)V9(04).
               05  IP-RATE           PIC 9(03)V9(02).
               05  GS-RATE           PIC 9(02)V9(02).
           02  SNFPR-VERSION         PIC X(09).

       01  CBSA-WI-TABLE.
           05  T-CBSA-DATA        OCCURS 8000
                                  INDEXED BY MA1 MA2 MA3.
               10  T-CBSA            PIC X(05).
               10  T-CBSA-EFFDATE    PIC X(08).
               10  T-CBSA-WAGEIND    PIC 9(02)V9(04).

       01  DUMMY-FLAG               PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           IF DUMMY-FLAG = 'Y'
               CALL 'SNFPR210' USING SNF-DATA HOLD-VARIABLES
           END-IF

           MOVE SPACES TO SNF-DATA
           MOVE SPACES TO HOLD-VARIABLES
           MOVE ZEROS  TO SNF-PAYMENT-RATE

      *    Test 3: urban CBSA, FY2021 transition date, AIDS=Y,
      *    facility-specific rate, supplemental wage index
           MOVE '0000'      TO SNF-MSA
           MOVE '11260'     TO SNF-CBSA
           MOVE 'Y'         TO SNF-SPEC-WI-IND
           MOVE 1.2500      TO SNF-SPEC-WI
           MOVE 'CCC22'     TO SNF-HIPPS-CODE
           MOVE '20'        TO SNF-FROM-CC
           MOVE '20'        TO SNF-FROM-YY
           MOVE '10'        TO SNF-FROM-MM
           MOVE '15'        TO SNF-FROM-DD
           MOVE '20'        TO SNF-THRU-CC
           MOVE '20'        TO SNF-THRU-YY
           MOVE '10'        TO SNF-THRU-MM
           MOVE '20'        TO SNF-THRU-DD
           MOVE '0'         TO SNF-FED-BLEND
           MOVE 0500.00     TO SNF-FACILITY-RATE
           MOVE 'Y'         TO SNF-SUPP-WI-IND
           MOVE 1.0500      TO SNF-SUPP-WI

      *    CBSA table: '11260' (Albuquerque)
           MOVE '11260'     TO T-CBSA       (1)
           MOVE '20201001'  TO T-CBSA-EFFDATE (1)
           MOVE 0.9700      TO T-CBSA-WAGEIND (1)

           CALL 'SNFDR211' USING SNF-DATA HOLD-VARIABLES CBSA-WI-TABLE

           DISPLAY 'SNF-RTC=[' SNF-RTC ']'
           DISPLAY 'SNF-PAYMENT-RATE=' SNF-PAYMENT-RATE
           DISPLAY 'REGION-IND=[' REGION-IND ']'
           DISPLAY 'AREA-WAGE-INDEX=' AREA-WAGE-INDEX
           DISPLAY 'IP-RATE=' IP-RATE
           DISPLAY 'GS-RATE=' GS-RATE
           DISPLAY 'AIDS-ADD-ON-IND=[' AIDS-ADD-ON-IND ']'
           DISPLAY 'SNFPR-VERSION=[' SNFPR-VERSION ']'
           STOP RUN.
