       IDENTIFICATION DIVISION.
       PROGRAM-ID. test_irdrv200.

      ******************************************************************
      * FY2020 IRF PPS Pricer parity test driver.
      * Calls IRDRV200 (master driver) which routes to IRCAL200 for
      * 2020 dates. Verifies byte-for-byte parity GnuCOBOL vs Ironclad.
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  BILL-NEW-DATA.
           05  B-NPI10.
               10  B-NPI8                   PIC X(08).
               10  B-NPI-FILLER             PIC X(02).
           05  B-PROVIDER-NO                PIC X(06).
           05  B-PATIENT-STATUS             PIC X(02).
           05  B-CMG-CODE                   PIC X(05).
           05  B-LOS                        PIC 9(03).
           05  B-COVERED-DAYS               PIC 9(03).
           05  B-LTR-DAYS                   PIC 9(02).
           05  B-SPEC-PYMT-IND              PIC X(01).
           05  B-DISCHARGE-DATE.
               10  B-DISCHG-CC              PIC 9(02).
               10  B-DISCHG-YY              PIC 9(02).
               10  B-DISCHG-MM              PIC 9(02).
               10  B-DISCHG-DD              PIC 9(02).
           05  B-COVERED-CHARGES            PIC 9(07)V9(02).
           05  FILLER                       PIC X(11).

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
               10  PPS-OUT-DAYS-USED        PIC 9(03).
               10  PPS-LTR-DAYS-USED        PIC 9(02).
               10  PPS-CALC-VERS-CD         PIC X(05).
               10  FILLER                   PIC X(36).
           05  FILLER                       PIC X(50).

       01  DUMMY-FLAG  PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
      *    Force companion discovery
           IF DUMMY-FLAG = 'Y'
              CALL 'IRCAL021' USING BILL-NEW-DATA
              CALL 'IRCAL031' USING BILL-NEW-DATA
              CALL 'IRCAL041' USING BILL-NEW-DATA
              CALL 'IRCAL051' USING BILL-NEW-DATA
              CALL 'IRCAL064' USING BILL-NEW-DATA
              CALL 'IRCAL070' USING BILL-NEW-DATA
              CALL 'IRCAL080' USING BILL-NEW-DATA
              CALL 'IRCAL090' USING BILL-NEW-DATA
              CALL 'IRCAL100' USING BILL-NEW-DATA
              CALL 'IRCAL110' USING BILL-NEW-DATA
              CALL 'IRCAL120' USING BILL-NEW-DATA
              CALL 'IRCAL130' USING BILL-NEW-DATA
              CALL 'IRCAL140' USING BILL-NEW-DATA
              CALL 'IRCAL150' USING BILL-NEW-DATA
              CALL 'IRCAL160' USING BILL-NEW-DATA
              CALL 'IRCAL170' USING BILL-NEW-DATA
              CALL 'IRCAL180' USING BILL-NEW-DATA
              CALL 'IRCAL190' USING BILL-NEW-DATA
              CALL 'IRCAL200' USING BILL-NEW-DATA
           END-IF

           MOVE LOW-VALUES TO BILL-NEW-DATA
           MOVE LOW-VALUES TO PPS-DATA-ALL

      *    FY2020 IRF claim (date 20200615 routes to IRCAL200)
           MOVE '12345678' TO B-NPI8
           MOVE '12'       TO B-NPI-FILLER
           MOVE '123456'   TO B-PROVIDER-NO
           MOVE '01'       TO B-PATIENT-STATUS
           MOVE 'A0101'    TO B-CMG-CODE
           MOVE 014        TO B-LOS
           MOVE 014        TO B-COVERED-DAYS
           MOVE 00         TO B-LTR-DAYS
           MOVE ' '        TO B-SPEC-PYMT-IND
           MOVE 20  TO B-DISCHG-CC
           MOVE 20  TO B-DISCHG-YY
           MOVE 06  TO B-DISCHG-MM
           MOVE 15  TO B-DISCHG-DD
           MOVE 0050000.00 TO B-COVERED-CHARGES

           CALL 'IRDRV200' USING BILL-NEW-DATA PPS-DATA-ALL

           DISPLAY 'PPS-RTC=[' PPS-RTC ']'
           DISPLAY 'PPS-MSA=[' PPS-MSA ']'
           DISPLAY 'PPS-TOTAL-PAY-AMT=' PPS-TOTAL-PAY-AMT
           DISPLAY 'PPS-FED-PAY-AMT=' PPS-FED-PAY-AMT
           DISPLAY 'PPS-OUTLIER-PAY-AMT=' PPS-OUTLIER-PAY-AMT
           DISPLAY 'PPS-CALC-VERS-CD=[' PPS-CALC-VERS-CD ']'
           STOP RUN.
