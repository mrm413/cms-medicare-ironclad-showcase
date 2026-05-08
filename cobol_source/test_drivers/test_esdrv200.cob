       IDENTIFICATION DIVISION.
       PROGRAM-ID. test_esdrv200.

      ******************************************************************
      * FY2020 ESRD PPS Pricer parity test driver.
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY BILLCPY.
       01  DUMMY-FLAG  PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           IF DUMMY-FLAG = 'Y'
              CALL 'ESCAL056' USING BILL-NEW-DATA
              CALL 'ESCAL062' USING BILL-NEW-DATA
              CALL 'ESCAL070' USING BILL-NEW-DATA
              CALL 'ESCAL071' USING BILL-NEW-DATA
              CALL 'ESCAL080' USING BILL-NEW-DATA
              CALL 'ESCAL091' USING BILL-NEW-DATA
              CALL 'ESCAL100' USING BILL-NEW-DATA
              CALL 'ESCAL117' USING BILL-NEW-DATA
              CALL 'ESCAL122' USING BILL-NEW-DATA
              CALL 'ESCAL130' USING BILL-NEW-DATA
              CALL 'ESCAL140' USING BILL-NEW-DATA
              CALL 'ESCAL151' USING BILL-NEW-DATA
              CALL 'ESCAL160' USING BILL-NEW-DATA
              CALL 'ESCAL170' USING BILL-NEW-DATA
              CALL 'ESCAL171' USING BILL-NEW-DATA
              CALL 'ESCAL180' USING BILL-NEW-DATA
              CALL 'ESCAL191' USING BILL-NEW-DATA
              CALL 'ESCAL200' USING BILL-NEW-DATA
           END-IF

           INITIALIZE BILL-NEW-DATA
           INITIALIZE PPS-DATA-ALL
           MOVE 2020      TO B-THRU-CCYY
           MOVE 06        TO B-THRU-MM
           MOVE 15        TO B-THRU-DD
           MOVE 1965      TO B-DOB-CCYY
           MOVE '00'      TO B-COND-CODE
           MOVE '821'     TO B-REV-CODE3
           MOVE 0017500   TO B-PATIENT-HEIGHT
           MOVE 0007500   TO B-PATIENT-WEIGHT
           MOVE '12345678' TO P-NPI
           MOVE '12'      TO P-NPI-FILLER
           MOVE '123456'  TO P-PROV-OSCAR
           MOVE '0000'    TO P-GEO-MSA
           MOVE '48060'   TO P-GEO-CBSA
           MOVE ' '       TO P-SPEC-PYMT-IND
           MOVE '01'      TO P-PROV-TYPE
           MOVE 12        TO B-CLAIM-NUM-DIALYSIS-SESSIONS
           MOVE 2020      TO B-LINE-ITEM-DT-SVC-CCYY
           MOVE 06        TO B-LINE-ITEM-DT-SVC-MM
           MOVE 01        TO B-LINE-ITEM-DT-SVC-DD
           MOVE 2018      TO B-DIALYSIS-START-DT-CCYY
           MOVE 01        TO B-DIALYSIS-START-DT-MM
           MOVE 15        TO B-DIALYSIS-START-DT-DD

           CALL 'ESDRV200' USING BILL-NEW-DATA PPS-DATA-ALL

           DISPLAY 'PPS-RTC=[' PPS-RTC ']'
           DISPLAY 'PPS-MSA=[' PPS-MSA ']'
           DISPLAY 'PPS-CBSA=[' PPS-CBSA ']'
           DISPLAY 'PPS-WAGE-ADJ-RATE=' PPS-WAGE-ADJ-RATE
           DISPLAY 'PPS-FINAL-PAY-AMT=' PPS-FINAL-PAY-AMT
           DISPLAY 'PPS-CALC-VERS-CD=[' PPS-CALC-VERS-CD ']'
           STOP RUN.
