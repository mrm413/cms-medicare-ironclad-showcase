       IDENTIFICATION DIVISION.
       PROGRAM-ID. test_escal151.

      ******************************************************************
      * FY2021 ESRD Calculator (ESCAL212) parity test driver.
      * Calls ESCAL212 directly with 5 LINKAGE records embedded as WS.
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY BILLCPY.
       COPY WAGECPY.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           INITIALIZE BILL-NEW-DATA
           INITIALIZE PPS-DATA-ALL
           INITIALIZE WAGE-NEW-RATE-RECORD
           INITIALIZE COM-CBSA-WAGE-RECORD
           INITIALIZE BUN-CBSA-WAGE-RECORD

      *    FY2021 dialysis claim
           MOVE '00'        TO B-COND-CODE
           MOVE '821'       TO B-REV-CODE3
           MOVE 0017500     TO B-PATIENT-HEIGHT
           MOVE 0007500     TO B-PATIENT-WEIGHT
           MOVE 2021        TO B-THRU-CCYY
           MOVE 06          TO B-THRU-MM
           MOVE 15          TO B-THRU-DD
           MOVE 1965        TO B-DOB-CCYY
           MOVE 03          TO B-DOB-MM
           MOVE 15          TO B-DOB-DD
           MOVE '12345678' TO P-NPI
           MOVE '12'       TO P-NPI-FILLER
           MOVE '123456'   TO P-PROV-OSCAR
           MOVE '0000'     TO P-GEO-MSA
           MOVE '48060'    TO P-GEO-CBSA
           MOVE ' '        TO P-SPEC-PYMT-IND
           MOVE '01'       TO P-PROV-TYPE
           MOVE 12         TO B-CLAIM-NUM-DIALYSIS-SESSIONS

      *    Wage records
           MOVE 11260       TO BUN-CBSA
           MOVE 20           TO BUN-CBSA-DATE-CC
           MOVE 21           TO BUN-CBSA-DATE-YY
           MOVE 06           TO BUN-CBSA-DATE-MM
           MOVE 15           TO BUN-CBSA-DATE-DD

           CALL 'ESCAL151' USING BILL-NEW-DATA
                                 PPS-DATA-ALL
                                 WAGE-NEW-RATE-RECORD
                                 COM-CBSA-WAGE-RECORD
                                 BUN-CBSA-WAGE-RECORD

           DISPLAY 'PPS-RTC=[' PPS-RTC ']'
           DISPLAY 'PPS-MSA=[' PPS-MSA ']'
           DISPLAY 'PPS-CBSA=[' PPS-CBSA ']'
           DISPLAY 'PPS-FINAL-PAY-AMT=' PPS-FINAL-PAY-AMT
           DISPLAY 'PPS-CALC-VERS-CD=[' PPS-CALC-VERS-CD ']'
           STOP RUN.
