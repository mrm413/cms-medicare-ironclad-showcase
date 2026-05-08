       IDENTIFICATION DIVISION.
       PROGRAM-ID. test_snfdr211_v2.

      ******************************************************************
      * Second parity test: rural CBSA, different dates, AIDS add-on,
      * supp wage index. Forces a different path through SNFDR211 +
      * SNFPR210 than test_snfdr211.
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

      *    Test 2: rural CBSA (1st 3 chars all spaces), older date,
      *    AIDS indicator, blend value 1.
           MOVE '0000'      TO SNF-MSA
           MOVE '   01'     TO SNF-CBSA
           MOVE ' '         TO SNF-SPEC-WI-IND
           MOVE 0           TO SNF-SPEC-WI
           MOVE 'BBB11'     TO SNF-HIPPS-CODE
           MOVE '20'        TO SNF-FROM-CC
           MOVE '19'        TO SNF-FROM-YY
           MOVE '06'        TO SNF-FROM-MM
           MOVE '15'        TO SNF-FROM-DD
           MOVE '20'        TO SNF-THRU-CC
           MOVE '19'        TO SNF-THRU-YY
           MOVE '06'        TO SNF-THRU-MM
           MOVE '20'        TO SNF-THRU-DD
           MOVE '1'         TO SNF-FED-BLEND
           MOVE 0           TO SNF-FACILITY-RATE
           MOVE ' '         TO SNF-SUPP-WI-IND
           MOVE 0           TO SNF-SUPP-WI

      *    Two table entries: a rural one matching, an urban distractor.
           MOVE '   01'     TO T-CBSA       (1)
           MOVE '20190601'  TO T-CBSA-EFFDATE (1)
           MOVE 0.8500      TO T-CBSA-WAGEIND (1)
           MOVE '   01'     TO T-CBSA       (2)
           MOVE '20180601'  TO T-CBSA-EFFDATE (2)
           MOVE 0.8200      TO T-CBSA-WAGEIND (2)

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
