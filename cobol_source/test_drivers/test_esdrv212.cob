       IDENTIFICATION DIVISION.
       PROGRAM-ID. test_esdrv212.

      ******************************************************************
      * FY2021 ESRD PPS Pricer parity test driver.
      * Calls ESDRV212 (master driver) which routes to ESCAL212 for
      * 2021 dates. Verifies byte-for-byte parity between GnuCOBOL and
      * Ironclad.
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY BILLCPY.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           MOVE LOW-VALUES TO BILL-NEW-DATA
           MOVE LOW-VALUES TO PPS-DATA-ALL

      *    BILL-PORTION: typical hemodialysis outpatient claim
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

      *    PROVIDER-SPECIFIC-FILE: minimal valid provider info
           MOVE '12345678'  TO P-NPI
           MOVE '12'        TO P-NPI-FILLER
           MOVE '123456'    TO P-PROV-OSCAR
           MOVE '0000'      TO P-GEO-MSA
           MOVE '48060'     TO P-GEO-CBSA
           MOVE ' '         TO P-SPEC-PYMT-IND
           MOVE '01'        TO P-PROV-TYPE
           MOVE 0           TO P-SPEC-WAGE-INDX
           MOVE 0           TO P-ESRD-RATE
           MOVE ' '         TO P-PROV-WAIVE-BLEND-PAY-INDIC
           MOVE ' '         TO P-PROV-LOW-VOLUME-INDIC
           MOVE ' '         TO P-PACIFIC-IS-TRUST-TERR
           MOVE ' '         TO P-QIP-REDUCTION
           MOVE ' '         TO P-SUPP-WI-IND
           MOVE 0           TO P-SUPP-WI

      *    BUNDLED-BILL-PORTION
           MOVE 12          TO B-CLAIM-NUM-DIALYSIS-SESSIONS
           MOVE 2021        TO B-LINE-ITEM-DT-SVC-CCYY
           MOVE 06          TO B-LINE-ITEM-DT-SVC-MM
           MOVE 01          TO B-LINE-ITEM-DT-SVC-DD
           MOVE 2018        TO B-DIALYSIS-START-DT-CCYY
           MOVE 01          TO B-DIALYSIS-START-DT-MM
           MOVE 15          TO B-DIALYSIS-START-DT-DD
           MOVE 0           TO B-TOT-PRICE-SB-OUTLIER
           MOVE 0           TO B-PAYER-ONLY-VC-Q8
           MOVE '  '        TO B-DATA-CODE
           MOVE '  '        TO B-PAYER-ONLY-VALUE-CODE
           MOVE 0           TO B-PAYER-ONLY-VC-QG-AMT

           CALL 'ESDRV212' USING BILL-NEW-DATA PPS-DATA-ALL

           DISPLAY 'P-GEO-CBSA=[' P-GEO-CBSA ']'
           DISPLAY 'PPS-RTC=[' PPS-RTC ']'
           DISPLAY 'PPS-MSA=[' PPS-MSA ']'
           DISPLAY 'PPS-CBSA=[' PPS-CBSA ']'
           DISPLAY 'PPS-WAGE-ADJ-RATE=' PPS-WAGE-ADJ-RATE
           DISPLAY 'PPS-FINAL-PAY-AMT=' PPS-FINAL-PAY-AMT
           DISPLAY 'PPS-CALC-VERS-CD=[' PPS-CALC-VERS-CD ']'
           STOP RUN.
